#ifndef __FLX_GC_H__
#define __FLX_GC_H__

#include <cstdlib>
#include "flx_gc_config.hpp"
#include "pthread_thread.hpp"
#include <string>

// we use an STL set to hold the collection of roots
#include <set>

namespace flx {
namespace gc {
namespace generic {
// Here are the types we refer to:

struct GC_EXTERN gc_shape_t;   // the shape of collectable objects
struct GC_EXTERN collector_t;  // the collector itself
struct GC_EXTERN allocator_t;  // the collector itself
struct GC_EXTERN offset_data_t; // private data for offset scanner
struct GC_EXTERN pointer_data_t; // description of a pointer

struct GC_EXTERN pointer_data_t
{
  void *pointer;                      //< candidate pointer
  void *head;                         //< head object
  unsigned long max_elements;         //< allocated slots
  unsigned long used_elements;        //< used slots
  gc_shape_t *shape;            //< shape
};

enum gc_shape_flags_t {
  gc_flags_default    = 0,            //< collectable and mobile
  gc_flags_immobile   = 1,            //< cannot be moved
  gc_flags_persistent = 2,            //< cannot be deallocated
  gc_flags_conservative = 4           //< scan whole object conservatively
};

/// Describes runtime object shape.
typedef void finaliser_t (collector_t*, void*); 
typedef void *scanner_t(collector_t*, gc_shape_t *, void *, unsigned long, int);
typedef ::std::string encoder_t (void *);
typedef ::std::size_t decoder_t(void *, char *, ::std::size_t);

struct GC_EXTERN gc_shape_t
{
  gc_shape_t *next_shape;   ///< pointer to next shape in list or NULL
  char const *cname;              ///< C++ typename
  ::std::size_t count;              ///< static array element count
  ::std::size_t amt;                ///< bytes allocated
  finaliser_t *finaliser;         ///< finalisation function
  void const *private_data;       ///< private data passed to scanner
  scanner_t *scanner;             ///< scanner function 
  encoder_t *encoder;             ///< encoder function 
  decoder_t *decoder;             ///< encoder function 
  gc_shape_flags_t flags;         ///< flags
  unsigned long allocations;
  unsigned long deallocations;
};

struct GC_EXTERN offset_data_t
{
  ::std::size_t n_offsets;
  ::std::size_t const *offsets;
};

GC_EXTERN scanner_t scan_by_offsets;


/*
 * The following template is provided as a standard wrapper
 * for C++ class destructors. The term std_finaliser<T>
 * denotes a function pointer to the wrapper for the destructor
 * of class T, which can be used as a finaliser in the shape
 * descriptor of a T. The client is cautioned than the order
 * of finalisation may not be what is expected. Finalisers
 * should be provided for all C++ objects managed by the Felix
 * collector and not refering to Felix objects,
 * but which contain pointers to other objects that need
 * to be deleted when the main object is destroyed;
 * for example a string class managing an array of char
 * requires its destructor be invoked to delete the managed
 * array, and so a finaliser wrapping the destructor must
 * be provided.
 *
 * C data types may, of course, also require destruction,
 * and Felix therefore can provide programmers with
 * the convenience of C++ destructors, even for C data types.
 */
template<class T>
void std_finaliser(collector_t*, void *t)
{
  static_cast<T*>(t) -> ~T();
}

/// Allocator abstraction.

struct allocator_t {
  bool debug;
  allocator_t():debug(false){}
  virtual void *allocate(::std::size_t)=0;
  virtual void deallocate(void *)=0;
  virtual ~allocator_t();
  void set_debug(bool d){debug=d;}
};

/// Collector abstraction.
struct GC_EXTERN collector_t
{
  bool debug;
  void *module_registry; 
  void set_debug(bool d){debug=d;}
  collector_t();
  virtual ~collector_t();
  virtual ::flx::pthread::thread_control_t *get_thread_control()const =0;
  virtual void register_pointer(void *q, int reclimit)=0;

  // These routines just provide statistics.
  unsigned long get_allocation_count()const {
    return v_get_allocation_count();
  }

  unsigned long get_root_count()const {
    return v_get_root_count();
  }

  unsigned long get_allocation_amt()const {
    return v_get_allocation_amt();
  }

  // Hooks for the supplied allocator, which operate in
  // terms of shape objects rather than raw memory amounts.
  void *allocate(gc_shape_t *shape, unsigned long x) {
    return v_allocate(shape,x);
  }

  // The mark and sweep collector algorithm.
  unsigned long collect() {
    //fprintf(stderr, "Collecting\n");
    unsigned long x = v_collect();
    //fprintf(stderr, "Collecting DONE\n");
    return x;
  }

  // Routines to add and remove roots.
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

  // Integrity check for the data structure being managed.
  // array management
  virtual void set_used(void *memory, unsigned long)=0;
  virtual void incr_used(void *memory, unsigned long)=0;
  virtual unsigned long get_used(void *memory)=0;
  virtual unsigned long get_count(void *memory)=0;
  virtual void *create_empty_array( gc_shape_t *shape, unsigned long count)=0;

  virtual pointer_data_t get_pointer_data(void *)=0;
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

  // It doesn't make any sense to copy collector objects
  // about.
  void operator=(collector_t const&);
  collector_t(collector_t const&);
};

// The gc_profile_t is a grab bag of controls related to the collector.
struct GC_EXTERN gc_profile_t {
  bool debug_driver;
  bool debug_allocations;     ///< allocator debug on/off
  bool debug_collections;     ///< collector debug on/off
  bool report_collections;     ///< collector debug on/off
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
    bool debug_driver_,
    bool debug_allocations_,
    bool debug_collections_,
    bool report_collections_,
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

}}} // end namespaces

/*
 * The following two routines are used to provide
 * C++ type safe heap allocation. There are no corresponding
 * delete routines, please use the destroy function.
 *
 * Note these routines are now placed
 * in the global namespace to accomodate Metrowerks
 * compiler on Mac OS.
 */
GC_EXTERN void *operator new
(
  ::std::size_t,
  flx::gc::generic::gc_profile_t &,
  flx::gc::generic::gc_shape_t &,
  bool
);

/*
 * Define an empty delete to make msvc happy.
 */
GC_EXTERN void operator delete(
  void*,
  flx::gc::generic::gc_profile_t &,
  flx::gc::generic::gc_shape_t &,
  bool
);

#endif
