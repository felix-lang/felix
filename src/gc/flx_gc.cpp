#include <cstdlib>
#include <cstdio>
#include <cassert>
#include "flx_gc.hpp"
#include "flx_exceptions.hpp"
#include "flx_gc_private.hpp"
#include <Judy.h>

#ifdef max
#undef max
#endif

namespace flx {
namespace gc {
namespace generic {

collector_t::collector_t() : debug(false), module_registry(0){}

gc_profile_t::gc_profile_t (
  bool debug_allocations_,
  bool debug_collections_,
  bool report_collections_,
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
  report_collections(report_collections_),
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
  if(debug_collections || report_collections) 
    fprintf(stderr,"[flx_gc:gc_profile_t] actually_collect\n");
  gc_counter = 0;
  unsigned long collected = collector->collect();
  unsigned long allocated = collector->get_allocation_amt();
  if (allocated > max_mem) throw flx::rtl::flx_out_of_memory_t();
  threshhold = std::max ( min_mem,
    (unsigned long) (free_factor * (double)allocated))
  ;
  if(debug_collections || report_collections)
  {
    unsigned long objs = collector->get_allocation_count();
    unsigned long roots = collector->get_root_count();
    fprintf(stderr, 
      "actually collected %ld objects, still allocated: %ld roots, %ld objects, %ld bytes\n",
      collected, roots, objs, allocated
    );
  }
  return collected;
}

void *gc_profile_t::allocate(
  flx::gc::generic::gc_shape_t const *shape,
  unsigned long count,
  bool allow_gc
)
{
  void *p = 0;
  ::std::size_t amt = count * shape->amt * shape->count;
  bool tried_collection = false;

  // if we would exceed the threshhold and collection is allowed, do it
  if (amt + collector->get_allocation_amt() > threshhold && allow_collection_anywhere && allow_gc)
  {
    if (report_collections)
      fprintf(stderr,"[flx_gc:gc_profile_t] Threshhold %ld would be exceeded, collecting\n", threshhold);
    actually_collect();
    if (report_collections)
      fprintf(stderr,"[flx_gc:gc_profile_t] New Threshhold %ld\n", threshhold);
    tried_collection = true;
  }

  // now try the allocation
  try {
    p = collector -> allocate(shape,count);
  }
  // if we ran out of physical memory
  catch (flx::rtl::flx_out_of_memory_t& exn) 
  { 
    if (debug_allocations || debug_collections || report_collections)
      fprintf(stderr,"[flx_gc:gc_profile_t] Out of physical memory\n");

    if (allow_collection_anywhere && allow_gc && !tried_collection)
    {
      actually_collect();
      tried_collection = true;
      try {
        p = collector -> allocate(shape,count);
      }
      catch (flx::rtl::flx_out_of_memory_t& exn) // fatal error
      {
         fprintf(stderr,"[flx_gc:gc_profile_t] Allocation failed [after forced collection]\n");
         throw exn;
      }
    }
    else 
    {
      fprintf(stderr,"[flx_gc:gc_profile_t] Allocation failed [collection not allowed or already tried]\n");
      throw exn; // fatal error
    }
  }

  assert (p);
  return p;
}

/*
 *  This is the default scanner for compiler generated RTTI objects.
 *  It uses an array of offsets into the object to tell where the pointers are.
 *  We must pass this routine the collector, the RTTI shape of the object,
 *  a pointer to the head (lowest byte) of the object, a count of the number
 *  of copies of the object are present consecutively, and a recursion limit.
 *
 *  The count is there because all Felix heap objects are varrays, even if they're
 *  merely length 1. Note that this dynamic array count is the number of used
 *  slots in the varray not the allocated length. Note also the elements of the
 *  varray can themselves be arrays with static lengths. The actual RTTI object
 *  describes a single element of the inner static length array, so we have to
 *  multiply the RTTI static length by the dynamic length.
 */
void *scan_by_offsets(collector_t *collector, gc_shape_t const *shape, void *p, unsigned long dyncount, int reclimit)
{
  Word_t fp = (Word_t)p;

  // calculate the absolute number of used array slots
  unsigned long n_used = dyncount  * shape->count;

  // find the array of offsets
  offset_data_t const *data = (offset_data_t const *)shape->private_data;
  ::std::size_t n_offsets = data->n_offsets;
  ::std::size_t const *offsets = data->offsets;

  // if the number of used slots is one and there is only one offset
  // then there is only one possible pointer in the object at the specified offset
  // so just return the value stored at that offset immediately
  if (n_used * n_offsets == 1) // tail rec optimisation
  {
      void **pq = (void**)(void*)((unsigned char*)fp + offsets[0]);
      void *q = *pq;
      if(q) return q; // tail rec optimisation
  }
  else
  // otherwise we have to scan through all the offsets in every array element
  for(unsigned long j=0; j<n_used; ++j)
  {
    for(unsigned int i=0; i<n_offsets; ++i)
    {
      void **pq = (void**)(void*)((unsigned char*)fp + offsets[i]);
      void *q = *pq;
      // instead of returning the pointer, register it for later processing
      if(q)
      {
        collector->register_pointer(q, reclimit);
      }
    }
    // on to the next array element
    fp=(Word_t)(void*)((unsigned char*)fp+shape->amt);
  }
  // return 0 to indicate we registered pointers, instead of returning just one.
  return 0;
}

}}} // end namespaces

// in global namespace now ..
//
// NOTE: Felix arrays are two dimensional. The shape.amt field is the size of
// one element. The shape.count field is the number of elements for a static
// array type. The dynamic length is for varrays, it is stored in a judy array
// associated with the array address. If there is nothing in the judy array,
// the dynamic length is one. C++ operator new allocates arrays of dynamic length 1. 
//
void *operator new(
  std::size_t amt,
  flx::gc::generic::gc_profile_t &gcp,
  flx::gc::generic::gc_shape_t const &shape,
  bool allow_gc
)
{
  if (amt != shape.amt * shape.count)
  {
    fprintf(stderr,"Shape size error: allocator size = %ld\n",(long)amt);
    fprintf(stderr,"Shape %s element size = %ld, element count = %ld\n",shape.cname,(long)shape.amt,(long)shape.count);
    abort();
  }
  void *p = gcp.allocate(&shape,1,allow_gc); // dynamic array count = 1
  return p;
}

void operator delete(
  void*,
  flx::gc::generic::gc_profile_t &,
  flx::gc::generic::gc_shape_t const &,
  bool
)
{
}


