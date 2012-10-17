#ifndef __FLX_PTHREAD_CONDV_H__
#define __FLX_PTHREAD_CONDV_H__
#include "flx_pthread_config.hpp"
#include "pthread_mutex.hpp"

#include "pthread_win_posix_condv_emul.hpp"

namespace flx { namespace pthread {

// ********************************************************
/// Condition Variable.
// ********************************************************
class PTHREAD_EXTERN flx_condv_t {
public:
  pthread_cond_t cv;        // be nice, don't touch
  flx_condv_t();
  ~flx_condv_t();
  void signal();
  void broadcast();
  void wait(flx_mutex_t*);
  int timedwait(flx_mutex_t*, timespec*);
  int timedwait(flx_mutex_t *m, double t);
};

}} // namespace pthread, flx
#endif

