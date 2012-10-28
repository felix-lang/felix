#ifndef __FLX_PTHREAD_WIN_POSIX_CONDV_EMUL_H__
#define __FLX_PTHREAD_WIN_POSIX_CONDV_EMUL_H__
// Note: no namespaces here!
// See http://www.cs.wustl.edu/~schmidt/win32-cv-1.html

#include "flx_pthread_config.hpp"
#if FLX_WIN32
#include <windows.h>

typedef HANDLE pthread_mutex_t;
typedef void pthread_mutexattr_t; // do NOT use them!
typedef void pthread_condattr_t; // do NOT use them!

struct timespec {
  unsigned long tv_sec;
  unsigned long tv_nsec;
};


struct pthread_cond_t
{
  int waiters_count_;
  // Number of waiting threads.

  CRITICAL_SECTION waiters_count_lock_;
  // Serialize access to <waiters_count_>.

  HANDLE sema_;
  // Semaphore used to queue up threads waiting for the condition to
  // become signaled.

  HANDLE waiters_done_;
  // An auto-reset event used by the broadcast/signal thread to wait
  // for all the waiting thread(s) to wake up and be released from the
  // semaphore.

  size_t was_broadcast_;
  // Keeps track of whether we were broadcasting or signaling.  This
  // allows us to optimize the code if we're just signaling.
};

// THIS IS SICK but there ain't no other way in C
#ifndef ETIMEDOUT
#define ETIMEDOUT WAIT_TIMEOUT
#endif
// looks like EAGAIN is available in minggw, but not in vs sdk.
#ifndef EAGAIN
#define EAGAIN WAIT_TIMEOUT
#endif

int PTHREAD_EXTERN pthread_mutex_init (pthread_mutex_t*, const pthread_mutexattr_t*);
int PTHREAD_EXTERN pthread_mutex_lock(pthread_mutex_t*);
int PTHREAD_EXTERN pthread_mutex_unlock(pthread_mutex_t*);
int PTHREAD_EXTERN pthread_mutex_destroy(pthread_mutex_t*);

int PTHREAD_EXTERN pthread_cond_init (pthread_cond_t*, const pthread_condattr_t*);
int PTHREAD_EXTERN pthread_cond_destroy(pthread_cond_t*);
int PTHREAD_EXTERN pthread_cond_wait (pthread_cond_t*, pthread_mutex_t*);
int PTHREAD_EXTERN pthread_cond_timedwait(pthread_cond_t*, pthread_mutex_t*, struct timespec const*);
int PTHREAD_EXTERN pthread_cond_uswait(pthread_cond_t*, pthread_mutex_t*, unsigned long us);
int PTHREAD_EXTERN pthread_cond_signal (pthread_cond_t*);
int PTHREAD_EXTERN pthread_cond_broadcast (pthread_cond_t*);


typedef HANDLE sem_t;

int PTHREAD_EXTERN sem_init(sem_t *sem, int pshared, unsigned int value);
int PTHREAD_EXTERN sem_wait(sem_t * sem);
int PTHREAD_EXTERN sem_trywait(sem_t * sem);
int PTHREAD_EXTERN sem_post(sem_t * sem);
int PTHREAD_EXTERN sem_getvalue(sem_t * sem, int * sval);
int PTHREAD_EXTERN sem_destroy(sem_t * sem);

#else
#include <errno.h>
#include <pthread.h>
#include <semaphore.h>
// emulate the native Window functionality
int PTHREAD_EXTERN pthread_cond_uswait( pthread_cond_t*, pthread_mutex_t*, unsigned long us);
#endif
#endif

