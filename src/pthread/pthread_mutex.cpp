#include "pthread_mutex.hpp"
#include <stdio.h>
#include <assert.h>

namespace flx { namespace pthread {
flx_mutex_t::flx_mutex_t(flx_mutex_t const&){} // uncopyable
void flx_mutex_t::operator=(flx_mutex_t const&){} // uncopyable

flx_mutex_t::flx_mutex_t() { pthread_mutex_init(&m, NULL); }

flx_mutex_t::~flx_mutex_t() { pthread_mutex_destroy(&m); }
void flx_mutex_t::lock() { pthread_mutex_lock(&m); }
void flx_mutex_t::unlock() { pthread_mutex_unlock(&m);}

flx_mutex_locker_t::flx_mutex_locker_t(flx_mutex_locker_t const&){} // uncopyable
void flx_mutex_locker_t::operator=(flx_mutex_locker_t const&){} // uncopyable
flx_mutex_locker_t::flx_mutex_locker_t(flx_mutex_t& pm) : p(&pm) { p->lock(); }
flx_mutex_locker_t::~flx_mutex_locker_t() { p->unlock(); }

}}

