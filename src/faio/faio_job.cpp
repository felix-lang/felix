#include "faio_job.hpp"

namespace flx { namespace faio {
// from flx_driver_request_base
bool job_t::start_async_op_impl(demux::demuxer& demux)
{
  //fprintf(stderr,"job_t: start async_op_impl\n");
  //printf("Adding task to worker queue\\n");
  // get worker fifo, add this task
  job_queue->add_worker_task(this);
  //printf("Task added\\n");
  return false;              // suspended
}
}}
