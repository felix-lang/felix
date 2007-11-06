#line 1388 "./lpsrc/flx_pthread.pak"
#ifndef __FLX_PTHREAD_MONITOR_H__
#define __FLX_PTHREAD_MONITOR_H__
#include <flx_pthread_config.hpp>
#include "pthread_mutex.hpp"
#include "pthread_condv.hpp"
#include "pthread_semaphore.hpp"

// interface for a consumer/producer queue. threads requesting a resource
// that isn't there block until one is available. push/pop re-entrant

namespace flx { namespace pthread {

// ********************************************************
/// A monitor is a concurrent version of a channel.
/// It matches up readers and writers in pairs,
/// synchronising transfer of one datum.
///
/// Unlike the bounded queue below, a monitor is a fully
/// synchronised unbuffered transfer, mediated by a full
/// handshake.
///
/// In particular, unlike the queue of size 1, the writer
/// cannot proceed until the reader sends an acknowledge
/// signal.
///
/// This logic matches that provides by schannels, but
/// across an asynchronous boundary.
// ********************************************************

class PTHREAD_EXTERN monitor_t {
  flx_mutex_t m;
  flx_mutex_t rm;
  flx_mutex_t wm;
  int dataput;
  int datagot;
  flx_condv_t ack;
  void *data;
public:
  monitor_t();
  ~monitor_t();
  void enqueue(void*);
  void* dequeue();
};

}} // namespace pthread, flx
#endif

