#ifndef __flx_world_H_
#define __flx_world_H_

#include "flx_gc.hpp"
#include "flx_collector.hpp"
#include "flx_dynlink.hpp"

// for async_sched
#include <list>
#include "flx_async.hpp"
#include "flx_sync.hpp"
#include "flx_world_config.hpp"
#include "flx_async_world.hpp"

namespace flx { namespace run {

class flx_world {
  bool debug;
  bool debug_driver;

  ::flx::gc::generic::allocator_t *allocator;

  ::flx::gc::collector::flx_collector_t *collector;

  ::flx::gc::generic::gc_profile_t *gcp;

  ::flx::pthread::thread_control_t *thread_control;

  ::flx::rtl::flx_dynlink_t *library;
  ::flx::rtl::flx_libinit_t instance;

  struct async_sched *async_handler;

  int explicit_dtor();
public:
  flx_config *c;
  flx_world(flx_config *); 
  int setup(int argc, char **argv);

  int teardown();

  // add/remove (current pthread, stack pointer) for garbage collection
  void begin_flx_code();
  void end_flx_code();

  // returns number of pending operations scheduled by svc_general
  // return error code < 0 otherwise
  // catches all known exceptions
  int run_until_blocked();
  int run_until_complete();

  void* ptf() { return instance.thread_frame; }	// for creating con_t
  void spawn_fthread(::flx::rtl::con_t *top);
};


}} // namespaces
#endif //__flx_world_H_
