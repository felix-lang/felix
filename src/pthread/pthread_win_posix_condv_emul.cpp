#include "pthread_win_posix_condv_emul.hpp"
#include <assert.h>
#if FLX_WIN32
#include <string.h>

// RF: unlike real pthread mutexes, windows mutexes are always recursive.
// that's annoying because I use deadlock as a debugging tool. the upshot
// is that recursively acquiring a mutex gives undefined results.
int pthread_mutex_init (pthread_mutex_t *m, const pthread_mutexattr_t*)
{
  *m = CreateMutex(NULL,FALSE,NULL);
  return 0;
}

int pthread_mutex_lock(pthread_mutex_t *m)
{
  WaitForSingleObject(*m,INFINITE);
  return 0;
}

int pthread_mutex_unlock(pthread_mutex_t *m)
{
  ReleaseMutex(*m);
  return 0;
}

int pthread_mutex_destroy(pthread_mutex_t *m)
{
  CloseHandle(*m);
  return 0;
}

int
pthread_cond_init
(
  pthread_cond_t *cv,
  const pthread_condattr_t *
)
{
  cv->waiters_count_ = 0;
  cv->was_broadcast_ = 0;
  cv->sema_ = CreateSemaphore
  (
    NULL,       // no security
    0,          // initially 0
    0x7fffffff, // max count .. (I hate limits .. but thats a lot of pthreads)
    NULL        // unnamed
  );
  InitializeCriticalSection (&cv->waiters_count_lock_);
  cv->waiters_done_ = CreateEvent
  (
    NULL,  // no security
    FALSE, // auto-reset
    FALSE, // non-signaled initially
    NULL   // unnamed
  );
  return 0;
}

int
pthread_cond_destroy(pthread_cond_t *cv)
{
  CloseHandle(cv->sema_);
  CloseHandle(cv->waiters_done_);
  return 0;
}

// returns ETIMEDOUT = WAIT_TIMEOUT
static int
private_cond_wait
(
  pthread_cond_t *cv,
  pthread_mutex_t *external_mutex,
  unsigned long ms // MilliSeconds
)
{
  // Avoid race conditions.
  EnterCriticalSection (&cv->waiters_count_lock_);
  cv->waiters_count_++;
  LeaveCriticalSection (&cv->waiters_count_lock_);

  // This call atomically releases the mutex and waits on the
  // semaphore until <pthread_cond_signal> or <pthread_cond_broadcast>
  // are called by another thread.
  int res = SignalObjectAndWait (*external_mutex, cv->sema_, ms, FALSE); // MilliSeconds

  // Reacquire lock to avoid race conditions.
  EnterCriticalSection (&cv->waiters_count_lock_);

  // We're no longer waiting...
  cv->waiters_count_--;

  // Check to see if we're the last waiter after <pthread_cond_broadcast>.
  int last_waiter = cv->was_broadcast_ && cv->waiters_count_ == 0;

  LeaveCriticalSection (&cv->waiters_count_lock_);

  // If we're the last waiter thread during this particular broadcast
  // then let all the other threads proceed.
  if (last_waiter)
    // This call atomically signals the <waiters_done_> event and waits until
    // it can acquire the <external_mutex>.  This is required to ensure fairness.
    SignalObjectAndWait (cv->waiters_done_, *external_mutex, INFINITE, FALSE);
  else
    // Always regain the external mutex since that's the guarantee we
    // give to our callers.
    WaitForSingleObject (*external_mutex, INFINITE);
  return res;
}

int
pthread_cond_wait
(
  pthread_cond_t *cv,
  pthread_mutex_t *external_mutex
)
{
  return private_cond_wait(cv,external_mutex,INFINITE);
}

// Posix is a pain in the butt here
// we have to get the current time and subtract it
// from the target time to get a duration
// the pain is that we probably wanted a duration
// and had to construct a target time by adding it
// to the current time
//
// to fix this we add the native Windows mode (a duration)
// to posix

int
pthread_cond_uswait
(
  pthread_cond_t *cv,
  pthread_mutex_t *external_mutex,
  unsigned long us // MicroSeconds
)
{

  // Windows waits in ms, ours in us
  return private_cond_wait(cv,external_mutex,us / 1000ul); // MilliSeconds
}

int
pthread_cond_timedwait
(
  pthread_cond_t *cv,
  pthread_mutex_t *external_mutex,
  struct timespec const *abstime
)
{
  unsigned long t1 = abstime->tv_sec * 1000ul + abstime->tv_nsec / 1000000ul; // MilliSeconds
  SYSTEMTIME tod;
  GetSystemTime(&tod);
  FILETIME ft;
  SystemTimeToFileTime(&tod,&ft);
  ULARGE_INTEGER now;  // so we can do some maths
  assert(sizeof(now) == sizeof(ft));
  memcpy(&now, &ft, sizeof(now));
  unsigned long t0 = now.QuadPart / 10; // us now
  unsigned long timeout = t1>t0 ? t1 - t0 : 0;
  return private_cond_wait(cv,external_mutex,timeout);
}

int
pthread_cond_signal (pthread_cond_t *cv)
{
  EnterCriticalSection (&cv->waiters_count_lock_);
  int have_waiters = cv->waiters_count_ > 0;
  LeaveCriticalSection (&cv->waiters_count_lock_);

  // If there aren't any waiters, then this is a no-op.
  if (have_waiters)
    ReleaseSemaphore (cv->sema_, 1, 0);
  return 0;
}

int
pthread_cond_broadcast (pthread_cond_t *cv)
{
  // This is needed to ensure that <waiters_count_> and <was_broadcast_> are
  // consistent relative to each other.
  EnterCriticalSection (&cv->waiters_count_lock_);
  int have_waiters = 0;

  if (cv->waiters_count_ > 0) {
    // We are broadcasting, even if there is just one waiter...
    // Record that we are broadcasting, which helps optimize
    // <pthread_cond_wait> for the non-broadcast case.
    cv->was_broadcast_ = 1;
    have_waiters = 1;
  }

  if (have_waiters) {
    // Wake up all the waiters atomically.
    ReleaseSemaphore (cv->sema_, cv->waiters_count_, 0);

    LeaveCriticalSection (&cv->waiters_count_lock_);

    // Wait for all the awakened threads to acquire the counting
    // semaphore.
    WaitForSingleObject (cv->waiters_done_, INFINITE);
    // This assignment is okay, even without the <waiters_count_lock_> held
    // because no other waiter threads can wake up to access it.
    cv->was_broadcast_ = 0;
  }
  else
    LeaveCriticalSection (&cv->waiters_count_lock_);
  return 0;
}

int sem_init(sem_t *sem, int pshared, unsigned int value)
{
  *sem = CreateSemaphore(NULL,value,0x7FFFFFFF,NULL);
  return 0;
}

int sem_wait(sem_t * sem)
{
  return WaitForSingleObject(*sem,INFINITE);
}

int sem_trywait(sem_t * sem)
{
  return WaitForSingleObject(*sem,0);
}

int sem_post(sem_t * sem)
{
  return ReleaseSemaphore(*sem,1,NULL);
}

int sem_getvalue(sem_t * sem, int * sval)
{
  LONG x;
  ReleaseSemaphore(*sem,0,&x);
  *sval = x;
  return 0;
}

int sem_destroy(sem_t * sem)
{
  return CloseHandle(*sem);
}


#else

//POSIX
#include <time.h>
#include <sys/time.h>

int pthread_cond_uswait(
  pthread_cond_t *cv,
  pthread_mutex_t *m,
  unsigned long us
)
{
  timeval tv;
  gettimeofday(&tv,NULL);
  unsigned long t0 = tv.tv_sec * 1000000uL + tv.tv_usec;
  unsigned long t1 = t0 + us;
  timespec ts;
  ts.tv_sec = t1 / 1000000uL;
  ts.tv_nsec = (t1 % 1000000uL) * 1000;
  return pthread_cond_timedwait(cv,m,&ts);
}

#endif

