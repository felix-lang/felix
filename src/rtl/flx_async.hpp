#line 2117 "./lpsrc/flx_rtl.pak"
#ifndef __FLX_ASYNC_H__
#define __FLX_ASYNC_H__
#include "flx_rtl_config.hpp"
#include "flx_rtl.hpp"

#ifdef BUILD_ASYNC
#define ASYNC_EXTERN FLX_EXPORT
#else
#define ASYNC_EXTERN FLX_IMPORT
#endif

// GLOBAL NAMESPACE!

class ASYNC_EXTERN async_hooker {
public:
  virtual flx::rtl::fthread_t *dequeue()=0;
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

#endif
