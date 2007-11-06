#ifndef __FLX_PTHREAD_COUNTER_H__
#define __FLX_PTHREAD_COUNTER_H__
#include "flx_pthread_config.hpp"
#include "pthread_mutex.hpp"
#include "pthread_condv.hpp"

namespace flx { namespace pthread {

// ********************************************************
/// Counter with zero signal
// ********************************************************
class PTHREAD_EXTERN flx_ts_counter_t {
  flx_mutex_t m;
  flx_condv_t c;
  long x;
  void operator=(flx_ts_counter_t const &);
  flx_ts_counter_t(flx_ts_counter_t const &);
public:
  flx_ts_counter_t();
  ~flx_ts_counter_t();
  long pre_incr(); // value AFTER increment
  long pre_decr(); // value AFTER decrement
  long post_incr(); // value BEFORE increment
  long post_decr(); // value BEFORE decrement

  long get();
  long set(long);   // returns argument
  long swap(long);  // returns old value
  long decr_pos(); // decrement if >0
  void wait_zero(); // wait for zero
  long operator++() { return pre_incr(); }
  long operator--() { return pre_decr(); }
  long operator++(int) { return post_incr(); }
  long operator--(int) { return post_decr(); }
  long operator*() { return get(); }
  long operator=(long a) { return set(a); }
};
}}

#endif

