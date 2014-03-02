#ifndef __FLX_SYNC_H__
#define __FLX_SYNC_H__

#include "flx_gc.hpp"
#include "flx_dynlink.hpp"
#include "flx_rtl.hpp"
#include <list>

namespace flx { namespace run {

// This class handles synchronous channel I/O and fthreads
struct RTL_EXTERN sync_sched {
  bool debug_driver;
  ::flx::gc::generic::collector_t *collector;
  ::std::list<flx::rtl::fthread_t*> *active;
  ::flx::rtl::fthread_t *ft;
  ::flx::rtl::_uctor_ *request;
  enum fstate_t { blocked, delegated };
  char const * get_fstate_desc();
  char const * get_fpc_desc();

  fstate_t fs;
  sync_sched (
    bool debug_driver_,
    ::flx::gc::generic::gc_profile_t *gcp_,
    ::std::list<flx::rtl::fthread_t*> *active_
  );
  void forget_current();
  void pop_current();
  void push_new(::flx::rtl::fthread_t*);
  void push_old(::flx::rtl::fthread_t*);

  void frun();
  void do_yield();
  void do_spawn_detached();
  void do_schedule_detached();
  void do_sread();
  void do_swrite();
  void do_multi_swrite();
  void do_kill();
};

}}

#endif
