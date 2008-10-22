#include <stdio.h>    // printf
#include "pthread_work_fifo.hpp"
namespace flx { namespace pthread {

int worker_fifo::get_nthreads() {
  flx_mutex_locker_t dummy(nlock);
  return nthreads;
}

void worker_fifo::set_nthreads(int n)
{
  flx_mutex_locker_t dummy(nlock);
  while(nthreads<n) start_worker_thread();
  while(nthreads>n) stop_worker_thread();
}

void worker_fifo::start_worker_thread()
{
  ++nthreads;
  //fprintf(stderr,"Spawn detached worker thread, count=%d\n",nthreads);
  flx_detached_thread_t().init(thread_start, this,NULL, NULL, NULL, NULL);
}

worker_fifo::worker_fifo(int n, int m) : nthreads(0), fifo(n)
{
  set_nthreads(m);
}

void
worker_fifo::stop_worker_thread()
{
  //fprintf(stderr,"Kill detached worker thread, count=%d\n",nthreads);
  --nthreads;
  add_worker_task(NULL);    // thread safe takedown.
}

worker_fifo::~worker_fifo()
{
  while(nthreads>0)stop_worker_thread();
  fifo.wait_until_empty();
}

// io thread entry point, passed this
void
worker_fifo::thread_start(void* udat)
{
  worker_fifo*  fio = (worker_fifo*)udat;
  while(fio->thread_loop_body()) ;
}

// dequeues one task and executes it, calling finished hook. interprets
// null task as a request to exit.
bool
worker_fifo::thread_loop_body()
{
  worker_task*  req = (worker_task*)fifo.dequeue();
  //fprintf(stderr,"dequeued worker_task (%p)\n", req);

  if(!req) return false;        // finished, got quit signal

  req->doit();
  req->finished();          // finish hook. I find this handy
  // Note: task not deleted. use finish hook if you want that

  return true;            // keep going
}

void
worker_fifo::add_worker_task(worker_task* task)
{
  //fprintf(stderr,"adding worker task %p\n",task);
  fifo.enqueue(task);         // don't worry, fifo is re-entrant
}

}}


