#include "flx_async.hpp"
#include "pthread_bound_queue.hpp"
#include "flx_rtl.hpp"
#include <cassert>
#include <stdio.h>

using namespace ::flx::rtl;
using namespace ::flx::pthread;
using namespace ::flx::async;

async_hooker::~async_hooker(){ }

namespace flx { namespace async {

// FINISHED NOTIFIER
finote_t::~finote_t(){}

// DERIVED NOTIFIER WHICH DOES FTHREAD WAKEUP
// BY ENQUEUING THE FTHREAD INTO THE READY QUEUE 
wakeup_fthread_t::wakeup_fthread_t(
  ::flx::pthread::bound_queue_t *q_a, 
  ::flx::rtl::fthread_t *f_a) 
: f(f_a), q(q_a) {}

// ASYNC HOOKER IMPLEMENTATION STAGE 1
// Introduces new virtual get_ready_queue().
class async_hooker_impl : public async_hooker {
public:
  virtual bound_queue_t *get_ready_queue()=0;
  ~async_hooker_impl() {}
  void handle_request(void *data,fthread_t *ss)
  {
    flx::async::flx_driver_request_base* dreq =
          (flx::async::flx_driver_request_base*)data
    ;
    finote_t *fn = new wakeup_fthread_t(get_ready_queue(),ss);
    dreq->start_async_op(fn);
  }
};


// ASYNC HOOKER IMPLEMENTATION STAGE 2
// Provides the ready queue and the dequeuing operations
class proto_async : public async_hooker_impl
{
    bound_queue_t async_ready;

public:
   proto_async(int n0, int n1, int m1, int n2, int m2) :
     async_ready(n0)
   {}

  ~proto_async(){}

  bound_queue_t *get_ready_queue() { return &async_ready; }

  fthread_t* dequeue()
  {
    return (fthread_t*)async_ready.dequeue();
  }
  fthread_t* maybe_dequeue()
  {
    return (fthread_t*)async_ready.maybe_dequeue();
  }
};


// DRIVER REQUEST BASE
// THIS IS USED TO BUILD REQUESTS
// PROVIDES DEFAULT NOTIFY_FINISHED ROUTINE WHICH USE FINOTE SIGNAL
// DO ASYNC OP JUST CALLS DRIVED CLASS DO_ASYNC_OP_IMPL
flx_driver_request_base::flx_driver_request_base() : fn(0) {}
flx_driver_request_base::~flx_driver_request_base() {}       // so destructors work

void flx_driver_request_base:: start_async_op(finote_t *fn_a)
{
  //fprintf(stderr,"start async op %p, set fn = %p\n",this,fn_a);
  assert(fn==0);
  fn = fn_a;
  bool completed =  start_async_op_impl();
  if(completed)
  {
    fprintf(stderr,"instant complete\n");
    notify_finished();
  }
  else
  {
    //fprintf(stderr,"Pending\n");
  }
}

void flx_driver_request_base:: notify_finished()
{
  //fprintf(stderr, "faio_req=%p, Notify finished %p\n", this,fn);
  assert(fn!=0);
  finote_t *fin = fn;
  fn=0;
  fin->signal();
  delete fin;
  //fprintf(stderr, "faio_req=%p, FINISHED\n",this);
}

}}

async_hooker *create_async_hooker(int n0,int n1,int m1,int n2,int m2) {
  return new ::flx::async::proto_async(n0,n1,m1,n2,m2);
}


