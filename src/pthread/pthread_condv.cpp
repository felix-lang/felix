#include "pthread_condv.hpp"
#include <stdio.h>        // printf debugging
#include <assert.h>

namespace flx { namespace pthread {

flx_condv_t::flx_condv_t() { 
  int res = pthread_cond_init(&cv, NULL); 
  #if !FLX_WIN32
  if(res==EINVAL) {
    // I suspect this is an error .. perhaps something got deleted
    fprintf(stderr,"pthread_cond_init returned EINVAL!");
  }
  #endif
}
flx_condv_t::~flx_condv_t() { 
  int res = pthread_cond_destroy(&cv); 
  #if !FLX_WIN32
  if(res==EINVAL) {
    // I suspect this is an error .. perhaps something got deleted
    fprintf(stderr,"pthread_cond_destroy returned EINVAL!");
  }
  #endif
}
void flx_condv_t::wait(flx_mutex_t *m) { 
  int res = pthread_cond_wait(&cv,&(m->m)); 
  #if !FLX_WIN32
  if(res==EINVAL) {
    // I suspect this is an error .. perhaps something got deleted
    fprintf(stderr,"pthread_cond_wait returned EINVAL!");
  }
  #endif
}
void flx_condv_t::signal() { 
  int res =  pthread_cond_signal(&cv);
  #if !FLX_WIN32
  if(res==EINVAL) {
    // I suspect this is an error .. perhaps something got deleted
    fprintf(stderr,"pthread_cond_signal returned EINVAL!");
  }
  #endif
}
void flx_condv_t::broadcast() { 
  int res = pthread_cond_broadcast(&cv); 
  #if !FLX_WIN32
  if(res==EINVAL) {
    // I suspect this is an error .. perhaps something got deleted
    fprintf(stderr,"pthread_cond_broadcast returned EINVAL!");
  }
  #endif
}
int flx_condv_t::timedwait(flx_mutex_t *m, timespec *t) {
  int res = pthread_cond_timedwait(&cv,&(m->m),t);
  #if !FLX_WIN32
  if(res==EINVAL) {
    // I suspect this is an error .. perhaps something got deleted
    fprintf(stderr,"pthread_cond_timedwait returned EINVAL!");
    return 0; // this is NOT an error!
  }
  #endif
  return res;
}
}}

