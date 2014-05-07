#ifndef __FLX_TS_COLLECTOR_H__
#define __FLX_TS_COLLECTOR_H__
#include "flx_collector.hpp"
#include "pthread_mutex.hpp"
#include "pthread_thread.hpp"

namespace flx {
namespace gc {
namespace collector {

/// Naive thread safe Mark and Sweep Collector.
struct GC_EXTERN flx_ts_collector_t :
  public flx::gc::collector::flx_collector_t
{
  flx_ts_collector_t(allocator_t *, flx::pthread::thread_control_t *);
  ~flx_ts_collector_t();

private:
  /// allocator
  void *v_allocate(gc_shape_t *ptr_map, unsigned long);

  /// collector (returns number of objects collected)
  unsigned long v_collect();

  // add and remove roots
  void v_add_root(void *memory);
  void v_remove_root(void *memory);

  // statistics
  unsigned long v_get_allocation_count()const;
  unsigned long v_get_root_count()const;
  unsigned long v_get_allocation_amt()const;

private:
  mutable flx::pthread::flx_mutex_t mut;
};


}}} // end namespaces

#endif
