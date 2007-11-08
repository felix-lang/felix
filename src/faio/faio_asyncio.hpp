#ifndef __FLX_FAIO_ASYNCIO_H__
#define __FLX_FAIO_ASYNCIO_H__
#include <flx_faio_config.hpp>
#include "pthread_sleep_queue.hpp"

#include "demux_demuxer.hpp"        // sel_param, demuxer base
#include "flx_rtl.hpp"

namespace flx { namespace faio {

struct FAIO_EXTERN finote_t
{
  virtual void signal()=0;
  virtual ~finote_t(){}
};

class FAIO_EXTERN wakeup_fthread_t : public finote_t
{
  rtl::fthread_t *f;
  pthread::sleep_queue_t *q;
public:
  wakeup_fthread_t(pthread::sleep_queue_t *q_a, rtl::fthread_t *f_a) : f(f_a), q(q_a) {}
  void signal () { q->enqueue(f); }
};


class FAIO_EXTERN flx_driver_request_base {
    finote_t *fn;
    virtual bool start_async_op_impl() = 0;
public:
    flx_driver_request_base() : fn(0) {}
    virtual ~flx_driver_request_base() {}       // so destructors work

    // returns finished flag (async may fail or immediately finish)
    void start_async_op(finote_t *fn_a);
    void notify_finished();
};

}} // namespace faio, flx
#endif  // __ASYNCIO__
