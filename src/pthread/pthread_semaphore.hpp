#ifndef __FLX_PTHREAD_SEMAPHORE_H__
#define __FLX_PTHREAD_SEMAPHORE_H__
#include "flx_pthread_config.hpp"
#include "pthread_mutex.hpp"

#include "pthread_win_posix_condv_emul.hpp"

namespace flx { namespace pthread {

// ********************************************************
/// Semaphore
// ********************************************************
class PTHREAD_EXTERN flx_semaphore_t {
  sem_t sem;
public:
  flx_semaphore_t(int n=0);
  ~flx_semaphore_t();
  void post();
  void operator++() { post(); }
  void wait();
  void operator--() { wait(); }
  int get();
  int operator*() { return get(); }

  /// atomic test and decrement if non-zero function.
  /// returns EAGAIN on failure to decrement.
  int trywait();
};

}} // namespace pthread, flx
#endif

