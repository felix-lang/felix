#include <assert.h>
#include "faio_asyncio.hpp"

namespace flx { namespace faio {

void flx_driver_request_base:: start_async_op(finote_t *fn_a)
{
  //fprintf(stderr,"start async op %p, set fn = %p\n",this,fn_a);
  fn = fn_a;
  bool completed =  start_async_op_impl();
  if(completed)
  {
    //fprintf(stderr,"instant complete\n");
    notify_finished();
  }
  else
  {
    //fprintf(stderr,"Pending\n");
  }
}

void flx_driver_request_base:: notify_finished()
{
  //fprintf(stderr, "faio_req=%p, Notify %p\n", this,fn);
  assert(fn!=0);
  finote_t *fin = fn;
  fn=0;
  fin->signal();
  delete fin;
  //fprintf(stderr, "faio_req=%p, FINISHED\n",this);
}

}}
