#include "flx_rtl_config.hpp"
#include "flx_ts_collector.hpp"

namespace flx {
namespace gc {
namespace collector {

flx_ts_collector_t::flx_ts_collector_t(allocator_t *a, flx::pthread::thread_control_t *tc) :
  flx_collector_t(a,tc)
{}

flx_ts_collector_t::~flx_ts_collector_t(){}

void *flx_ts_collector_t::v_allocate(gc_shape_t *ptr_map, unsigned long x) {
  flx::pthread::flx_mutex_locker_t l(mut);
  return impl_allocate(ptr_map,x);
}

unsigned long flx_ts_collector_t::v_collect() {
  // NO MUTEX
  return impl_collect();
}

void flx_ts_collector_t::v_add_root(void *memory) {
  flx::pthread::flx_mutex_locker_t l(mut);
  impl_add_root(memory);
}

void flx_ts_collector_t::v_remove_root(void *memory) {
  flx::pthread::flx_mutex_locker_t l(mut);
  impl_remove_root(memory);
}

unsigned long flx_ts_collector_t::v_get_allocation_count()const {
  flx::pthread::flx_mutex_locker_t l(mut);
  return impl_get_allocation_count();
}

unsigned long flx_ts_collector_t::v_get_root_count()const {
  flx::pthread::flx_mutex_locker_t l(mut);
  return impl_get_root_count();
}

unsigned long flx_ts_collector_t::v_get_allocation_amt()const {
  flx::pthread::flx_mutex_locker_t l(mut);
  return impl_get_allocation_amt();
}


}}} // end namespaces
