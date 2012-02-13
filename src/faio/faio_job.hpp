#ifndef __FLX_FAIO_JOB_H__
#define __FLX_FAIO_JOB_H__
#include <flx_faio_config.hpp>
#include "demux_demuxer.hpp"
#include "pthread_work_fifo.hpp"
#include "flx_async.hpp"

namespace flx { namespace faio {
class FAIO_EXTERN job_t:
  public ::flx::async::flx_driver_request_base,
  public pthread::worker_task
{
   pthread::worker_fifo* job_queue;
public:

  // from flx_driver_request_base
  bool start_async_op_impl(flx::demux::demuxer& demux);
};
}}

#endif
