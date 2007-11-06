#line 1208 "./lpsrc/flx_pthread.pak"
#include "pthread_counter.hpp"
#include <stdio.h>

namespace flx { namespace pthread {


flx_ts_counter_t::flx_ts_counter_t() : x(0) {}

flx_ts_counter_t::~flx_ts_counter_t() {
  wait_zero();
}

long flx_ts_counter_t::pre_incr() {
  flx_mutex_locker_t l(m);
  ++x;
  return x;
}

long flx_ts_counter_t::pre_decr() {
  flx_mutex_locker_t l(m);
  --x;
  if(x==0) c.signal();
  return x;
}

long flx_ts_counter_t::post_incr() {
  flx_mutex_locker_t l(m);
  ++x;
  return x+1;
}

long flx_ts_counter_t::post_decr() {
  flx_mutex_locker_t l(m);
  --x;
  if(x==0) c.signal();
  return x+1;
}

long flx_ts_counter_t::decr_pos() {
  flx_mutex_locker_t l(m);
  if(x>0)--x;
  if(x==0) c.signal();
  return x;
}

long flx_ts_counter_t::get() {
  flx_mutex_locker_t l(m);
  return x;
}

long flx_ts_counter_t::set(long a) {
  flx_mutex_locker_t l(m);
  x = a;
  return x;
}

long flx_ts_counter_t::swap(long a) {
  flx_mutex_locker_t l(m);
  long tmp = x;
  x = a;
  if(x==0) c.signal();
  return tmp;
}

void flx_ts_counter_t::wait_zero() {
  flx_mutex_locker_t l(m);
  while(1){
    if(x==0)return;
    c.wait(&m);
  }
}

}}

