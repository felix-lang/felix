#line 2164 "./lpsrc/flx_rtl.pak"
#include "flx_async.hpp"
#include "pthread_sleep_queue.hpp"
#include "flx_rtl.hpp"
#include "demux_demuxer.hpp"
#include "faio_asyncio.hpp"
#include "pthread_thread.hpp"

using namespace flx::rtl;
using namespace flx::demux;
using namespace flx::pthread;
using namespace flx::faio;


class async_hooker_impl : public async_hooker {
public:
  void handle_request(void *data,fthread_t *ss);
  virtual sleep_queue_t *get_ready_queue()=0;
  ~async_hooker_impl();
};


#include "faio_asyncio.hpp"

async_hooker::~async_hooker(){
 //fprintf(stderr,"Deleted async_hooker\n");
}

async_hooker_impl::~async_hooker_impl(){
  //fprintf(stderr,"Deleted async_hooker_impl\n");
}

void async_hooker_impl::handle_request(void *data,fthread_t *ft)
{
  flx::faio::flx_driver_request_base* dreq =
        (flx::faio::flx_driver_request_base*)data
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
    sleep_queue_t async_active;

public:
   proto_async(int n0, int n1, int m1, int n2, int m2) :
     async_active(n0)
   {
   }

  ~proto_async(){
    //fprintf(stderr,"Deleting proto async\n");
  }
  sleep_queue_t *get_ready_queue() { return &async_active; }
  fthread_t* dequeue()
  {
    return (fthread_t*)async_active.dequeue();
  }
};

async_hooker *create_async_hooker(int n0,int n1,int m1,int n2,int m2) {
  return new proto_async(n0,n1,m1,n2,m2);
}
