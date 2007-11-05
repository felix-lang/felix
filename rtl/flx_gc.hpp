#line 86 "./lpsrc/flx_gc.pak"
#ifndef __FLX_GC_H__
#define __FLX_GC_H__

#include <cstdlib>
#include "flx_gc_config.hpp"
#include "pthread_thread.hpp"

// we use an STL set to hold the collection of roots
#include <set>

namespace flx {
namespace gc {
namespace generic {
// Here are the types we refer to:

struct GC_EXTERN gc_shape_t;   // the shape of collectable objects
struct GC_EXTERN collector_t;  // the collector itself
struct GC_EXTERN allocator_t;  // the collector itself

enum gc_shape_flags_t {
  gc_flags_default    = 0,            //< collectable and mobile
  gc_flags_immobile   = 1,            //< cannot be moved
  gc_flags_persistent = 2             //< cannot be deallocated
};

/// Describes runtime object shape.
struct GC_EXTERN gc_shape_t
{
  gc_shape_t *next_shape;         ///< pointer to next shape in list or NULL
  char const *cname;              ///< C++ typename
  std::size_t count;              ///< array element count
  std::size_t amt;                ///< bytes allocated
  void (*finaliser)(collector_t*, void*);  ///< finalisation function
  std::size_t n_offsets;          ///< number of offsets
  std::size_t *offsets;           ///< actual offsets
  gc_shape_flags_t flags;         ///< flags
};
#line 147 "./lpsrc/flx_gc.pak"
template<class T>
void std_finaliser(collector_t*, void *t)
{
  static_cast<T*>(t) -> ~T();
}

#line 156 "./lpsrc/flx_gc.pak"

/// Allocator abstraction.

struct allocator_t {
  bool debug;
  allocator_t():debug(false){}
  virtual void *allocate(std::size_t)=0;
  virtual void deallocate(void *)=0;
  virtual void *reallocate(void *, std::size_t)=0;
  virtual ~allocator_t(){};
  void set_debug(bool d){debug=d;}
};

#line 172 "./lpsrc/flx_gc.pak"

/// Collector abstraction.
struct GC_EXTERN collector_t
{
  bool debug;
  void set_debug(bool d){debug=d;}
  collector_t();
  virtual ~collector_t(){}
  virtual flx::pthread::thread_control_t *get_thread_control()const =0;

#line 185 "./lpsrc/flx_gc.pak"
  unsigned long get_allocation_count()const {
    return v_get_allocation_count();
  }

  unsigned long get_root_count()const {
    return v_get_root_count();
  }

  unsigned long get_allocation_amt()const {
    return v_get_allocation_amt();
  }

#line 201 "./lpsrc/flx_gc.pak"
  void *allocate(gc_shape_t *shape, unsigned long x) {
    return v_allocate(shape,x);
  }

#line 208 "./lpsrc/flx_gc.pak"
  unsigned long collect() {
    //fprintf(stderr, "Collecting\n");
    unsigned long x = v_collect();
    //fprintf(stderr, "Collecting DONE\n");
    return x;
  }

#line 218 "./lpsrc/flx_gc.pak"
  void add_root(void *memory) {
    v_add_root(memory);
  }

  void remove_root(void *memory) {
    v_remove_root(memory);
  }

  void free_all_mem() {
    //fprintf(stderr,"Dispatching to free all mem\n");
    v_free_all_mem();
  }

  void finalise(void *frame) {
    v_finalise(frame);
  }

#line 238 "./lpsrc/flx_gc.pak"
  //array management
  virtual void set_used(void *memory, unsigned long)=0;
  virtual void incr_used(void *memory, unsigned long)=0;
  virtual unsigned long get_used(void *memory)=0;
  virtual unsigned long get_count(void *memory)=0;
  virtual void *create_empty_array( gc_shape_t *shape, unsigned long count)=0;
private:
  virtual unsigned long v_get_allocation_count()const=0;
  virtual unsigned long v_get_root_count()const=0;
  virtual unsigned long v_get_allocation_amt()const=0;
  virtual void *v_allocate(gc_shape_t *shape, unsigned long)=0;
  virtual void v_finalise(void *fp)=0;
  virtual unsigned long v_collect()=0;
  virtual void v_add_root(void *memory)=0;
  virtual void v_remove_root(void *memory)=0;
  virtual void v_free_all_mem()=0;

#line 259 "./lpsrc/flx_gc.pak"
  void operator=(collector_t const&);
  collector_t(collector_t const&);
};

#line 267 "./lpsrc/flx_gc.pak"

struct GC_EXTERN gc_profile_t {
  bool debug_allocations;     ///< allocator debug on/off
  bool debug_collections;     ///< collector debug on/off
  bool allow_collection_anywhere; ///< enable collect on allocate

  unsigned long gc_freq;      ///< how often to collect
  unsigned long gc_counter;   ///< counter to check if time to collect

  unsigned long min_mem;      ///< min memory before collection
  unsigned long max_mem;      ///< throw out of memory if above here
  unsigned long threshhold;   ///< collection trigger point
  double free_factor;         ///< reset threshhold to used memory
                              ///< by this factor after collection

  unsigned long collections;  ///< number of collections done
  bool finalise;              ///< whether Felix should collect on exit
  flx::gc::generic::collector_t *collector;

  unsigned long maybe_collect(); ///< function which maybe collects
  unsigned long actually_collect(); ///< function which actually collects

  void *allocate(
    flx::gc::generic::gc_shape_t *shape,
    unsigned long count,
    bool allow_gc
  );

  gc_profile_t (
    bool debug_allocations_,
    bool debug_collections_,
    bool allow_collection_anywhere_,
    unsigned long gc_freq_,
    unsigned long min_mem_,
    unsigned long max_mem_,
    double free_factor_,
    bool finalise_,
    flx::gc::generic::collector_t *collector
  );
  ~gc_profile_t();
};


#line 311 "./lpsrc/flx_gc.pak"

}}} // end namespaces

#line 324 "./lpsrc/flx_gc.pak"
/// Allocate collectable object
GC_EXTERN void *operator new
(
  std::size_t,
  flx::gc::generic::gc_profile_t &,
  flx::gc::generic::gc_shape_t &,
  bool
);
#endif

