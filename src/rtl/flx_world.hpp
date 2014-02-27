#ifndef __flx_world_H_
#define __flx_world_H_

#include "flx_gc.hpp"
#include "flx_collector.hpp"
#include "flx_dynlink.hpp"

// for doflx_data
#include <list>
#include "flx_async.hpp"
#include "flx_sync.hpp"

namespace flx { namespace run {

// TODO: move to own file
class flx_config {
public:
  bool  debug;

  bool debug_threads;
  bool debug_allocations;
  bool debug_collections;
  bool report_collections;

  bool debug_driver;
  bool finalise;

  unsigned long gc_freq;
  unsigned long min_mem;
  unsigned long max_mem;

  double free_factor;

  bool allow_collection_anywhere;

  bool static_link;
  char *filename; // expected to live forever
  char **flx_argv;
  int flx_argc;

  // TODO: fn up in macro area
  int init(int argc, char **argv);

// interface for drivers. there's more, create_frame, etc
  create_async_hooker_t *ptr_create_async_hooker;

  typedef ::flx::rtl::flx_dynlink_t *(*link_library_t)(flx_config *c);
  typedef void (*init_ptr_create_async_hooker_t)(flx_config *, bool debug_driver);
  typedef int (*get_flx_args_config_t)(int argc, char **argv, flx_config* c);

  link_library_t link_library;
  init_ptr_create_async_hooker_t init_ptr_create_async_hooker;
  get_flx_args_config_t get_flx_args_config;

  flx_config (link_library_t, init_ptr_create_async_hooker_t, get_flx_args_config_t); 


};

struct doflx_data;

class flx_world {
  bool debug;
  bool debug_driver;

  ::flx::gc::generic::allocator_t *allocator;

  ::flx::gc::collector::flx_collector_t *collector;

  ::flx::gc::generic::gc_profile_t *gcp;

  ::flx::pthread::thread_control_t *thread_control;

  ::flx::rtl::flx_dynlink_t *library;
  ::flx::rtl::flx_libinit_t instance;

  doflx_data *dfd;

  int explicit_dtor();
public:
  flx_config *c;
  flx_world(flx_config *); 
  int setup(int argc, char **argv);

  int teardown();

  // add/remove (current pthread, stack pointer) for garbage collection
  void begin_flx_code();
  void end_flx_code();

  bool run_until_blocked();

  void* ptf() { return instance.thread_frame; }	// for creating con_t
  void spawn_fthread(::flx::rtl::con_t *top);
};


// This class handles pthreads and asynchronous I/O
// It shares operations with sync_state_t by interleaving
// based on state variables.
//
struct doflx_data
{
  flx_world *world;
  bool debug_driver;
  ::flx::gc::generic::gc_profile_t *gcp;
  std::list<::flx::rtl::fthread_t*> *active;
  ::flx::pthread::thread_control_t *thread_control;

  unsigned long async_count;
  async_hooker* async;
  sync_state_t ss;  // (d, gcp, active), (ft, request), (pc, fs)

  doflx_data(
    flx_world *world_arg, 
    bool d, 
    flx::gc::generic::gc_profile_t *g, 
    ::std::list<::flx::rtl::fthread_t*> *a, 
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

  ~doflx_data();

  bool doflx();
private:
  bool do_async();
};


}} // namespaces
#endif //__flx_world_H_
