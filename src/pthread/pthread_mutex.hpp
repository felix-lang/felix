#ifndef __FLX_PTHREAD_MUTEX_H__
#define __FLX_PTHREAD_MUTEX_H__
#include "flx_pthread_config.hpp"

#include "pthread_win_posix_condv_emul.hpp"

namespace flx { namespace pthread {

// ********************************************************
/// Mutex.
// ********************************************************
class PTHREAD_EXTERN flx_mutex_t {
  flx_mutex_t(flx_mutex_t const&); // uncopyable
  void operator=(flx_mutex_t const&); // uncopyable
public:
  pthread_mutex_t m;        // be nice, don't touch
  flx_mutex_t();
  ~flx_mutex_t();
  void lock();
  void unlock();
};

class PTHREAD_EXTERN flx_mutex_locker_t {
  flx_mutex_t* p;
  flx_mutex_locker_t(flx_mutex_locker_t const&); // uncopyable
  void operator=(flx_mutex_locker_t const&); // uncopyable
public:
  flx_mutex_locker_t(flx_mutex_t& pm);
  ~flx_mutex_locker_t();
};

}} // namespace pthread, flx
#endif

