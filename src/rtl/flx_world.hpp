#ifndef __flx_world_H_
#define __flx_world_H_

#include "flx_gc.hpp"
#include "flx_collector.hpp"
#include "flx_dynlink.hpp"

// for doflx_data
#include <list>
#include "flx_async.hpp"
#include "flx_sync.hpp"

namespace flx { namespace rtl {

// don't want this here, but can't figure out how to predeclare
struct doflx_data
{
  bool debug_driver;
  flx::gc::generic::gc_profile_t *gcp;
  std::list<fthread_t*> *active;
  flx::pthread::thread_control_t *thread_control;

  unsigned long async_count;
  async_hooker* async;
  run::sync_state_t ss;  // (d, gcp, active), (ft, request), (pc, fs)

  doflx_data(bool d, flx::gc::generic::gc_profile_t *g, std::list<fthread_t*> *a, flx::pthread::thread_control_t *tc)
  : debug_driver(d), gcp(g), active(a), thread_control(tc),
  async_count(0),
  async(NULL),
  ss(debug_driver, gcp, active)
  {}

  ~doflx_data();

  bool doflx();
private:
  bool do_async();
};

class flx_world {
  bool debug;
  bool debug_driver;

  flx::gc::generic::allocator_t *allocator;

  flx::gc::collector::flx_collector_t *collector;

  flx::gc::generic::gc_profile_t *gcp;

  flx::pthread::thread_control_t *thread_control;

  flx_dynlink_t *library;
  flx_libinit_t instance;

  doflx_data *dfd;

  int explicit_dtor();
public:
  int setup(int argc, char **argv);

  int teardown();

  bool run_until_blocked();
};

}} // namespaces
#endif //__flx_world_H_
