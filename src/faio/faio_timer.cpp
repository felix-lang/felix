#include "faio_timer.hpp"

using namespace flx::demux;
namespace flx { namespace faio {
bool
sleep_request::start_async_op_impl()
{
  //fprintf(stderr,"Sleep: start async_op_impl %p\n",this);
  sleepers->add_sleep_request(this, delta);
  return false;   // no wakeup
}

void sleep_request::fire() {
  //fprintf (stderr,"FIRE req=%p\n",this);
  notify_finished();
}

}}
