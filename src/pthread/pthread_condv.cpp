#include "pthread_condv.hpp"
#include <stdio.h>        // printf debugging
#include <assert.h>

namespace flx { namespace pthread {

flx_condv_t::flx_condv_t() { pthread_cond_init(&cv, NULL); }
flx_condv_t::~flx_condv_t() { pthread_cond_destroy(&cv); }
void flx_condv_t::wait(flx_mutex_t *m) { pthread_cond_wait(&cv,&(m->m)); }
void flx_condv_t::signal() { pthread_cond_signal(&cv);}
void flx_condv_t::broadcast() { pthread_cond_broadcast(&cv); }
int flx_condv_t::timedwait(flx_mutex_t *m, timespec *t) {
  int res = pthread_cond_timedwait(&cv,&(m->m),t);
  #if !FLX_WIN32
  if(res==EINVAL) return 0; // this is NOT an error!
  #endif
  return res;
}
}}

