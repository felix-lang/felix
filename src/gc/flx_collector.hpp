#ifndef __FLX_COLLECTOR_H__
#define __FLX_COLLECTOR_H__
#include <cstddef>
#include "flx_gc.hpp"
#include "flx_gc_private.hpp"
#include <map>
#include "pthread_thread.hpp"
#include <Judy.h>

namespace flx {
namespace gc {
namespace collector {
using namespace generic;

struct GC_EXTERN malloc_free;
struct GC_EXTERN flx_collector_t;

/// Allocator using malloc and free.
struct GC_EXTERN malloc_free : public virtual allocator_t
{
  void *allocate(::std::size_t);
  void deallocate(void *);
  ~malloc_free();
};


/// Naive Mark and Sweep Collector.
struct GC_EXTERN flx_collector_t : public collector_t
{
  flx_collector_t(allocator_t *, flx::pthread::thread_control_t *);
  ~flx_collector_t();

  // RF: added to allow implementation of non-leaky drivers.
  void impl_free_all_mem(); // clear all roots, sweep.

  void set_used(void *memory, unsigned long);
  void incr_used(void *memory, unsigned long);
  unsigned long get_used(void *memory);
  unsigned long get_count(void *memory);
  void *create_empty_array( gc_shape_t *shape, unsigned long count);
  gc_shape_t *get_shape(void *memory);
  flx::pthread::thread_control_t *get_thread_control()const;
  void register_pointer(void *q, int reclimit);
  ::flx::gc::generic::pointer_data_t get_pointer_data(void *);

protected:

  /// allocator
  void *impl_allocate(gc_shape_t *ptr_map, unsigned long);

  /// collector (returns number of objects collected)
  unsigned long impl_collect();

  // add and remove roots
  void impl_add_root(void *memory);
  void impl_remove_root(void *memory);

  //
  void check();

  // statistics
  unsigned long impl_get_allocation_count()const;
  unsigned long impl_get_root_count()const;
  unsigned long impl_get_allocation_amt()const;
  void impl_finalise(void *fp);

private:
  /// allocator
  void *v_allocate(gc_shape_t *ptr_map, unsigned long);

  /// collector (returns number of objects collected)
  unsigned long v_collect();

  // add and remove roots
  void v_add_root(void *memory);
  void v_remove_root(void *memory);
  void v_free_all_mem();

  // statistics
  unsigned long v_get_allocation_count()const;
  unsigned long v_get_root_count()const;
  unsigned long v_get_allocation_amt()const;

private:
  void judyerror(char const*);
  unsigned long allocation_count;
  unsigned long root_count;
  unsigned long allocation_amt;


  void unlink(void *frame);
  void v_finalise(void *frame);
  void post_delete(void *frame);
  void delete_frame(void *frame);
  unsigned long reap();

  void mark(pthread::memory_ranges_t*);
  unsigned long sweep(); // calls scan_object

  typedef std::map<void *,unsigned long, std::less<void *> > rootmap_t;
  rootmap_t roots;
  bool parity;
  allocator_t *allocator;
  flx::pthread::thread_control_t *thread_control;


  // JudyL array and error object
  void *j_shape;
  void *j_nalloc;
  void *j_nused;
public:
  void scan_object(void *memory, int reclimit);
  void *j_tmp;
  JError_t je;
};

}}} // end namespaces
#endif
