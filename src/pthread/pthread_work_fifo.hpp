#ifndef __FLX_PTHREAD_WORKER_FIFO_H__
#define __FLX_PTHREAD_WORKER_FIFO_H__
#include "flx_pthread_config.hpp"
#include "pthread_thread.hpp"
#include "pthread_mutex.hpp"
#include "pthread_bound_queue.hpp"

namespace flx { namespace pthread {

/// Class of jobs to be queued in fifo for execution.
class PTHREAD_EXTERN worker_task
{
public:
  virtual ~worker_task() {}   // c++ should do this automatically

  /// function called by worker thread to carry out user job
  virtual void doit() = 0;

  /// function called by worker thread after doit() is completed
  /// used to notify job completion
  virtual void finished() = 0; // finished hook (mi serve start gancia?)
};

/// Job scheduler, executes jobs in turn from queue
/// Does not delete dequeued jobs
class PTHREAD_EXTERN worker_fifo
{
  flx_mutex_t nlock;
  int nthreads;                 /// scheduled number of threads
  bound_queue_t fifo;

  static void thread_start(void*); // thread entry point, passed this
  bool thread_loop_body();      // returns keep going flag
  void stop_worker_thread();
  void start_worker_thread();

public:
  worker_fifo(int n, int m);   /// n: Q bound, m: # of threads
  ~worker_fifo();
  void add_worker_task(worker_task* task);
  int get_nthreads();
  void set_nthreads(int);
};

}} // namespace pthread, flx
#endif  // __WORKER_FIFO__

