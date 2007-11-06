#include "pthread_thread.hpp"
#include <stdio.h>
#include <string.h>  // strerror
#include <cstdlib>
#include <setjmp.h>
#include <functional> // less
#include <assert.h>

namespace flx { namespace pthread {

flx_native_thread_t get_current_native_thread() { return pthread_self(); }

static void *get_stack_pointer() { unsigned long x; return &x; }

extern "C" void *flx_pthread_start_wrapper(void *e)
{
  void *stack_base = get_stack_pointer();
  tstart_t *ehd = (tstart_t*)e;
  thread_control_t *tc = ehd -> tc;
  if(tc && tc->debug)
    fprintf(stderr,"Spawned Thread %lx start stack base = %p, tc=%p\n",get_current_native_thread(),stack_base, tc);
  if(tc)tc->add_thread(stack_base);
  void (*sr)(void*)=ehd->sr;
  void *cd = ehd->cd;
  if(ehd->spawner_lock)
  {
    flx_mutex_locker_t dummy(*ehd->spawner_lock);
    //fprintf(stderr,"Thread %lx acquired mutex\n", get_current_native_thread());
    //fprintf(stderr,"Thread %lx notifying spawner it has registered itself\n", get_current_native_thread());
    *ehd->spawner_flag=true;
    ehd->spawner_cond->broadcast();
    //fprintf(stderr,"Thread %lx releasing mutex\n", get_current_native_thread());
  }
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
  return NULL;
}


// ---- detached threads ----------

flx_detached_thread_t::flx_detached_thread_t(flx_detached_thread_t const&){} // uncopyable
void flx_detached_thread_t::operator=(flx_detached_thread_t const&){} // uncopyable

int
flx_detached_thread_t::init(void (*start)(void*), void* udat, thread_control_t *tc,
  flx_mutex_t * m,flx_condv_t *c,bool *flag)
{
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  int res = pthread_create(&thr, &attr, flx_pthread_start_wrapper,
    new tstart_t(start, udat, tc, m,c,flag));
  if(res)
  {
     fprintf(stderr, "WARNING: flx_detached_thread_t: pthread_create failed: %s\n",
       strerror(res));
  }
  pthread_attr_destroy(&attr);
  return res;
}

flx_detached_thread_t::~flx_detached_thread_t() { }
flx_detached_thread_t::flx_detached_thread_t() { }

// ---- joinable threads ----------
flx_thread_t::flx_thread_t(flx_thread_t const&){} // uncopyable
void flx_thread_t::operator=(flx_thread_t const&){} // uncopyable

int
flx_thread_t::init(void (*start)(void*), void* udat, thread_control_t*tc)
{
  int res = pthread_create(&thr, NULL, flx_pthread_start_wrapper,
    new tstart_t(start, udat, tc,NULL,NULL,NULL));
  if(res)
  {
     fprintf(stderr, "WARNING: flx_thread_t: pthread_create failed: %s\n",
       strerror(res));
  }
  return res;
}

void flx_thread_t::join() {
  int res = pthread_join(thr, NULL);
  if(res)
  {
     fprintf(stderr, "flx_thread_t: FATAL: pthread_join failed: %s\n",
       strerror(res));
     std::exit(1);
  }
}

flx_thread_t::~flx_thread_t() { }
flx_thread_t::flx_thread_t() { }

// ---- joinable thread wrapper ----------

flx_thread_wrapper_t::flx_thread_wrapper_t(flx_thread_wrapper_t const&){} // uncopyable
void flx_thread_wrapper_t::operator=(flx_thread_wrapper_t const&){} // uncopyable

flx_thread_wrapper_t::flx_thread_wrapper_t(void (*start)(void*), void* udat, thread_control_t*tc)
{
  int res = thread.init(start,udat,tc);
  {
    if(res)
    {
       fprintf(stderr, "FATAL: flx_thread_wapper_t: flx_thread_t.init failed: %s\n",
         strerror(res));
       exit(1);
    }
  }
}

flx_thread_wrapper_t::~flx_thread_wrapper_t() { thread.join(); }
}}

