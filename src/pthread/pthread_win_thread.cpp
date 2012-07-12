#include "pthread_thread.hpp"
#if FLX_WIN32
#include <stdio.h>
#include <cstdlib>

namespace flx { namespace pthread {

flx_native_thread_t get_current_native_thread() { return GetCurrentThread(); }

#if defined(_MSC_VER)
#  pragma warning(push)
#  pragma warning(disable:4172)
#endif //defined(_MSC_VER)

static void *get_stack_pointer() { unsigned long x; return &x; }

#if defined(_MSC_VER)
#  pragma warning(pop)
#endif //defined(_MSC_VER)

DWORD WINAPI flx_pthread_start_wrapper(LPVOID e)
{
  void *stack_base = get_stack_pointer();
  tstart_t *ehd = (tstart_t*)e;
  thread_control_t *tc = ehd -> tc;
  if(tc && tc->debug)
    fprintf(stderr,"Spawned Thread %lx start stack base = %p, tc=%p\n",GetCurrentThreadId(),stack_base, tc);
  if(tc)tc->add_thread(stack_base);
  void (*sr)(void*)=ehd->sr;
  void *cd = ehd->cd;
  delete ehd;
  if(tc)tc->yield();
  try {
    (*sr)(cd);
  }
  catch (...) {
    fprintf(stderr,"Uncaught exception in thread\n");
    std::exit(1);
  }
  if(tc)tc->remove_thread();
  return 0;
}


// ---- detached threads ----------

flx_detached_thread_t::flx_detached_thread_t(flx_detached_thread_t const&){} // uncopyable
void flx_detached_thread_t::operator=(flx_detached_thread_t const&){} // uncopyable

// returns -1 on failure with error in GetLastError, 0 if all good.
int
flx_detached_thread_t::init(void (*start)(void*), void *lParam, thread_control_t *tc,
  flx_mutex_t * m,flx_condv_t *c,bool *flag)
{
  DWORD thread_id = 0;
  thr = (HANDLE)CreateThread(NULL, 0,
    (LPTHREAD_START_ROUTINE)flx_pthread_start_wrapper,
    new tstart_t(start,lParam, tc, m, c, flag), 0,
    &thread_id
  );

  if(!thr)
  {
    DWORD err = GetLastError();
    fprintf(stderr, "flx_detached_thread_t: CreateThread failed: %i\n", err);
    return err;
  }
  return 0;
}

flx_detached_thread_t::~flx_detached_thread_t() { CloseHandle(thr); }
flx_detached_thread_t::flx_detached_thread_t() { }

// ---- joinable threads ----------
flx_thread_t::flx_thread_t(flx_thread_t const&){} // uncopyable
void flx_thread_t::operator=(flx_thread_t const&){} // uncopyable


flx_thread_t::flx_thread_t() { }
flx_thread_t::~flx_thread_t() { }

// this should be idempotent
void
flx_thread_t::join()
{
  // Let's try and wait for the thread to finish, however first I have to
  // tell it to finish up.

  DWORD  wait_res = WaitForSingleObject(thr, INFINITE);

  // will this give me my return status? how do I get that?
  if(WAIT_FAILED == wait_res)
  {
    fprintf(stderr,"WARNING: thread wait failed (%li)\n", GetLastError());
  }

  // I've already tried waiting on the  thread's #include <stdlib> exit
  if(!CloseHandle(thr))
  {
    fprintf(stderr,"FATAL: failed to delete thread (%li)\n", GetLastError());
    std::exit(1);
  }
}

// returns -1 on failure with error in GetLastError, 0 if all good.
int
flx_thread_t::init(void (*fn)(void*), void *lParam, thread_control_t *tc)
{
  DWORD thread_id = 0;
  thr= (HANDLE)CreateThread(NULL, 0,
    (LPTHREAD_START_ROUTINE)flx_pthread_start_wrapper,
    new tstart_t(fn,lParam, tc,NULL,NULL,NULL), 0,
    &thread_id
  );

  if(!thr)
  {
    DWORD err = GetLastError();
    fprintf(stderr, "WARNING: flx_thread_t: CreateThread failed: %i\n", err);
    return err;
  }

  return 0;
}

// ---- joinable thread wrapper ----------
flx_thread_wrapper_t::flx_thread_wrapper_t(void (*f)(void*), void *lParam, thread_control_t*tc)
{
  int res = thread.init(f,lParam,tc);
  if(res)
  {
    fprintf(stderr,"flx_thread_wrapper_t: FATAL: flx_thread_t.init failed\n");
    std::exit(1);
  }
}
flx_thread_wrapper_t::~flx_thread_wrapper_t() { thread.join(); }

}}

#endif
