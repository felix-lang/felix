#include "flx_async.hpp"
#include "pthread_bound_queue.hpp"
#include "flx_rtl.hpp"
#include <cassert>
#include <stdio.h>

using namespace ::flx::rtl;
using namespace ::flx::pthread;
using namespace ::flx::async;

async_hooker::~async_hooker(){
 //fprintf(stderr,"Deleted async_hooker\n");
}

namespace flx { namespace async {

finote_t::~finote_t(){}
wakeup_fthread_t::wakeup_fthread_t(::flx::pthread::bound_queue_t *q_a, ::flx::rtl::fthread_t *f_a) : f(f_a), q(q_a) {}

class async_hooker_impl : public async_hooker {
public:
  void handle_request(void *data,fthread_t *ss);
  virtual bound_queue_t *get_ready_queue()=0;
  ~async_hooker_impl();
};


async_hooker_impl::~async_hooker_impl(){
  //fprintf(stderr,"Deleted async_hooker_impl\n");
}

void async_hooker_impl::handle_request(void *data,fthread_t *ft)
{
  flx::async::flx_driver_request_base* dreq =
        (flx::async::flx_driver_request_base*)data
  ;

  //fprintf(stderr,"Request object at %p\n",dreq);
  // RF hates the flag this function returns .. might
  // mask a race condition, get rid of it

  finote_t *fn = new wakeup_fthread_t(get_ready_queue(),ft);
  //fprintf(stderr,"async hooker dispatching request %p, fn = %p\n",dreq,fn);
  dreq->start_async_op(fn);
}

class proto_async : public async_hooker_impl
{
    bound_queue_t async_ready;

public:
   proto_async(int n0, int n1, int m1, int n2, int m2) :
     async_ready(n0)
   {
   }

  ~proto_async(){
    //fprintf(stderr,"Deleting proto async\n");
  }
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


