#line 343 "./lpsrc/flx_gc.pak"

#include "flx_gc.hpp"
#include "flx_rtl.hpp"
#include <cstdlib>
#include <cstdio>
#include <cassert>
#include "flx_gc_private.hpp"

#ifdef max
#undef max
#endif

namespace flx {
namespace gc {
namespace generic {

collector_t::collector_t() : debug(false) {}

gc_profile_t::gc_profile_t (
  bool debug_allocations_,
  bool debug_collections_,
  bool allow_collection_anywhere_,
  unsigned long gc_freq_,
  unsigned long min_mem_,
  unsigned long max_mem_,
  double free_factor_,
  bool finalise_,
  flx::gc::generic::collector_t *collector_
) :
  debug_allocations(debug_allocations_),
  debug_collections(debug_collections_),
  allow_collection_anywhere(allow_collection_anywhere_),
  gc_freq(gc_freq_),
  gc_counter(0),
  min_mem(min_mem_),
  max_mem(max_mem_),
  threshhold(min_mem_),
  free_factor(free_factor_),
  collections(0),
  finalise(finalise_),
  collector(collector_)
{
}

gc_profile_t::~gc_profile_t() { }

unsigned long gc_profile_t::maybe_collect() {
  ++gc_counter;
  if(debug_collections) fprintf(stderr,"Maybe collect?\n");
  if (gc_counter < gc_freq) return 0;
  if(collector->get_allocation_amt() < threshhold) return 0;
  return actually_collect();
}

unsigned long gc_profile_t::actually_collect() {
  if(debug_collections) fprintf(stderr,"Actually collect\n");
  gc_counter = 0;
  unsigned long collected = collector-> collect();
  unsigned long allocated = collector->get_allocation_amt();
  if (allocated > max_mem) throw flx::rtl::flx_out_of_memory_t();
  threshhold = std::max ( min_mem,
    (unsigned long) (free_factor * (double)allocated))
  ;
  if(debug_collections)
    fprintf(stderr, "actually collected %ld bytes\n",collected);
  return collected;
}

void *gc_profile_t::allocate(
  flx::gc::generic::gc_shape_t *shape,
  unsigned long amt,
  bool allow_gc
)
{
  //fprintf(stderr,"gc_profile_t::allocate(): allow_collection_anywhere=%s, allow_gc=%s\n",
  //  (allow_collection_anywhere?"True":"False"), (allow_gc?"True":"false")
  //);
  if (allow_collection_anywhere && allow_gc)
  {
    maybe_collect();
    try {
      return collector -> allocate(shape,amt);
    }
    catch (flx::rtl::flx_out_of_memory_t&) {
      actually_collect();
      return collector -> allocate(shape,amt);
    }
  }
  else
    return collector -> allocate(shape,amt);
}

}}} // end namespaces

// in global namespace now ..
void *operator new(
  std::size_t amt,
  flx::gc::generic::gc_profile_t &gcp,
  flx::gc::generic::gc_shape_t &shape,
  bool allow_gc
)
{
  if (amt != shape.amt)
  {
    fprintf(stderr,"Shape size error: allocator size = %ld\n",amt);
    fprintf(stderr,"Shape %s size = %ld\n",shape.cname,shape.amt);
    abort();
  }
  void *p = gcp.allocate(&shape,1,allow_gc);
  return p;
}

