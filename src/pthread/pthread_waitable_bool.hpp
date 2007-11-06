#ifndef __FLX_PTHREAD_WAIT_BOOL_H__
#define __FLX_PTHREAD_WAIT_BOOL_H__
#include "flx_pthread_config.hpp"
#include "pthread_mutex.hpp"  // mutexes
#include "pthread_condv.hpp"  // condition var for same

namespace flx { namespace pthread {

// a waitable boolean.
class PTHREAD_EXTERN waitable_bool {
  flx::pthread::flx_mutex_t cv_lock;       // to work with the condition var
  flx::pthread::flx_condv_t finished_cond;
  bool finished;   // might seem redundant, but that's how CVs work.
public:
  waitable_bool();

  void wait_until_true();
  void signal_true();
};

}} // namespace pthread, flx
#endif // __FLX_PTHREAD_WAIT_BOOL_H__

