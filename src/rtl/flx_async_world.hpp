#ifndef __flx_async_world_H_
#define __flx_async_world_H_

#include "flx_gc.hpp"
#include "flx_collector.hpp"
#include "flx_sync.hpp"

namespace flx { namespace run {

// This class handles pthreads and asynchronous I/O
// It shares operations with sync_sched by interleaving
// based on state variables.
//
struct async_sched
{
  enum block_flag_t {block, ret};

  struct flx_world *world;
  bool debug_driver;
  ::flx::gc::generic::gc_profile_t *gcp;
  ::std::list< ::flx::rtl::fthread_t*> *active;
  ::flx::pthread::thread_control_t *thread_control;

  unsigned long async_count;
  async_hooker* async;
  sync_sched ss;  // (d, gcp, active), (ft, request), (pc, fs)

  async_sched(
    flx_world *world_arg, 
    bool d, 
    ::flx::gc::generic::gc_profile_t *g, 
    ::std::list< ::flx::rtl::fthread_t*> *a, 
    ::flx::pthread::thread_control_t *tc
  ) : 
    world(world_arg), 
    debug_driver(d), 
    gcp(g), 
    active(a), 
    thread_control(tc),
    async_count(0),
    async(NULL),
    ss(debug_driver, gcp, active)
  {}

  ~async_sched();

  int prun(block_flag_t);
  void do_spawn_pthread();
  void do_general();

  void external_multi_swrite(::flx::rtl::schannel_t *, void *data);
private:
  bool schedule_queued_fthreads(block_flag_t);
};


}} // namespaces
#endif //__flx_async_world_H_
