#ifndef __FLX_FAIO_TIMER_H__
#define __FLX_FAIO_TIMER_H__
#include <flx_faio_config.hpp>

#include "demux_demuxer.hpp"        // sel_param, demuxer base
#include "faio_asyncio.hpp"
#include "demux_timer_queue.hpp"

#include "flx_rtl.hpp"

namespace flx { namespace faio {


// sleeping
class FAIO_EXTERN sleep_request
  : public flx_driver_request_base, public demux::sleep_task
{
  demux::timer_queue *sleepers;
  double      delta;
public:
  sleep_request() {}        // flx linkage

  sleep_request(demux::timer_queue *sleepers_a, double d) :
    sleepers(sleepers_a), delta(d)
  {}

  // from driver request
  bool start_async_op_impl();

  void fire();

};

}} // namespace faio, flx
#endif
