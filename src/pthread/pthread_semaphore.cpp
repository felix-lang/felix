#line 1372 "./lpsrc/flx_pthread.pak"
#include "pthread_semaphore.hpp"
#include <stdio.h>        // printf debugging
#include <assert.h>

namespace flx { namespace pthread {
flx_semaphore_t::flx_semaphore_t(int n) { sem_init(&sem, 0, n); }
flx_semaphore_t::~flx_semaphore_t() { sem_destroy(&sem); }
void flx_semaphore_t::wait() { sem_wait(&sem); }
int flx_semaphore_t::trywait() { return sem_trywait(&sem); }
void flx_semaphore_t::post() { sem_post(&sem); }
int flx_semaphore_t::get(){ int x; sem_getvalue(&sem,&x); return x; }

}}

