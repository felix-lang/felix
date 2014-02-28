#ifndef __FLX_ASYNC_H__
#define __FLX_ASYNC_H__
#include "flx_rtl_config.hpp"
#include "flx_rtl.hpp"
#include "pthread_bound_queue.hpp"

#ifdef BUILD_ASYNC
#define ASYNC_EXTERN FLX_EXPORT
#else
#define ASYNC_EXTERN FLX_IMPORT
#endif

// GLOBAL NAMESPACE!

class ASYNC_EXTERN async_hooker {
public:
  virtual flx::rtl::fthread_t *dequeue()=0;
  virtual flx::rtl::fthread_t *maybe_dequeue()=0;
  virtual void handle_request(void *data, flx::rtl::fthread_t *ss)=0;
  virtual ~async_hooker();
};

typedef
async_hooker *
create_async_hooker_t
(
  int n0,   // bound on resumable thread queue
  int n1,   // bound on general input job queue
  int m1,   // number of threads in job pool
  int n2,   // bound on async fileio job queue
  int m2    // number of threads doing async fileio
);

extern "C" {
ASYNC_EXTERN async_hooker *
create_async_hooker
(
  int n0,   // bound on resumable thread queue
  int n1,   // bound on general input job queue
  int m1,   // number of threads in job pool
  int n2,   // bound on async fileio job queue
  int m2    // number of threads doing async fileio
);
}

namespace flx { namespace async {
struct ASYNC_EXTERN finote_t
{
  virtual void signal()=0;
  virtual ~finote_t();
};

class ASYNC_EXTERN wakeup_fthread_t : public finote_t
{
  ::flx::rtl::fthread_t *f;
  ::flx::pthread::bound_queue_t *q;
public:
  wakeup_fthread_t(::flx::pthread::bound_queue_t *q_a, ::flx::rtl::fthread_t *f_a);
  void signal () { q->enqueue(f); }
};


class ASYNC_EXTERN flx_driver_request_base {
    finote_t *fn;
    virtual bool start_async_op_impl() = 0;
public:
    flx_driver_request_base();
    virtual ~flx_driver_request_base(); // so destructors work

    // returns finished flag (async may fail or immediately finish)
    void start_async_op(finote_t *fn_a);
    void notify_finished();
};

}}

#endif
