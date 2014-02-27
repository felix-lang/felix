#ifndef __FLX_SYNC_H__
#define __FLX_SYNC_H__

#include "flx_gc.hpp"
#include "flx_dynlink.hpp"
#include "flx_rtl.hpp"
#include <list>

namespace flx { namespace run {

enum fstate_t { terminated, blocked, delegated };
enum fpc_t { next_fthread_pos, next_request_pos };

RTL_EXTERN char const * get_fstate_desc(fstate_t);
RTL_EXTERN char const * get_fpc_desc(fpc_t);

struct RTL_EXTERN sync_state_t {
  bool debug_driver;
  ::flx::gc::generic::collector_t *collector;
  ::std::list<flx::rtl::fthread_t*> *active;
  ::flx::rtl::fthread_t *ft;
  ::flx::rtl::_uctor_ *request;
  fpc_t pc;
  fstate_t fs;
  sync_state_t (
    bool debug_driver_,
    ::flx::gc::generic::gc_profile_t *gcp_,
    ::std::list<flx::rtl::fthread_t*> *active_
  );
  void frun();
  void do_yield();
  void do_spawn_detached();
  void do_sread();
  void do_swrite();
  void do_multi_swrite();
  void do_kill();
};

}}

#endif
