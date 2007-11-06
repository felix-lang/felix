#include "pthread_waitable_bool.hpp"

namespace flx { namespace pthread {

waitable_bool::waitable_bool()
  : finished(false)
{
  // nothing
}

// can be called from any thread
void
waitable_bool::wait_until_true()
{
  flx::pthread::flx_mutex_locker_t    locker(cv_lock);

  // wait for the wakeup to say it's finished
  while(!finished)
  {
    finished_cond.wait(&cv_lock);
  }
}

void
waitable_bool::signal_true()
{
  { // the mutex is required for the memory barrier..
    flx::pthread::flx_mutex_locker_t    locker(cv_lock);
    finished = true;
  }
  finished_cond.signal();
  // do absolutely NOTHING here as a typical use of this class is to
  // wait for a thread exit and then destruct its resources, which could
  // very well include this object. boom.
}

} }

