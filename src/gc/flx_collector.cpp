#include <cstdlib>
#include <map>
#include <limits.h>
#include <cassert>
#include <cstdio>
#include <cstddef>
#include "flx_rtl_config.hpp"
#include "flx_collector.hpp"
#include "flx_exceptions.hpp"

//#include "flx_rtl.hpp"
namespace flx {
namespace gc {
namespace collector {

static int mcount FLX_UNUSED = 0;

void *malloc_free::allocate(::std::size_t amt)
{
  void *p = malloc(amt);
  if(debug)
    fprintf(stderr,"[gc] Malloc %ld bytes, address = %p\n",amt,p);
  if(p)return p;
  else {
    fprintf(stderr,"[gc] Felix: Malloc out of memory, blk=%ld\n",long(amt));
    throw flx::rtl::flx_out_of_memory_t();
  }
}

void malloc_free::deallocate(void *p)
{
  if(debug)
    fprintf(stderr,"[gc] Free %p\n",p);
  free(p);
}

void *flx_collector_t::v_allocate(gc_shape_t const *ptr_map, unsigned long x) {
  return impl_allocate(ptr_map, x);
}

void flx_collector_t::v_finalise(void *frame) {
  impl_finalise(frame);
}

unsigned long flx_collector_t::v_collect() {
  // NO MUTEX
  return impl_collect();
}

void flx_collector_t::v_add_root(void *memory) {
  impl_add_root(memory);
}

void flx_collector_t::v_remove_root(void *memory) {
  impl_remove_root(memory);
}

void flx_collector_t::v_free_all_mem() {
  //fprintf(stderr, "Dispatching to impl free all mem\n");
  impl_free_all_mem();
}

unsigned long flx_collector_t::v_get_allocation_count()const {
  return impl_get_allocation_count();
}

unsigned long flx_collector_t::v_get_root_count()const {
  return impl_get_root_count();
}

unsigned long flx_collector_t::v_get_allocation_amt()const {
  return impl_get_allocation_amt();
}

unsigned long flx_collector_t::impl_get_allocation_count()const
{
  return allocation_count;
}

unsigned long flx_collector_t::impl_get_root_count()const
{
  return root_count;
}

unsigned long flx_collector_t::impl_get_allocation_amt()const
{
  return allocation_amt;
}


flx_collector_t::flx_collector_t(allocator_t *a, pthread::thread_control_t *tc)
  :
  allocation_count(0)
  ,root_count(0)
  ,allocation_amt(0)
  ,parity(false)
  ,allocator(a)
  ,thread_control(tc)
  ,j_shape(0)
  ,j_nalloc(0)
  ,j_nused(0)
  ,j_tmp(0)
{}

flx::pthread::thread_control_t *flx_collector_t::get_thread_control()const
{
  return thread_control;
}

void flx_collector_t::judyerror(char const *loc)
{
  fprintf(stderr, "[gc] JUDY ERROR %d in %s\n",je.je_Errno,loc);
  abort();
}

void * flx_collector_t::impl_allocate(gc_shape_t const *shape, unsigned long nobj)
{
  // calculate how much memory to request
  ::std::size_t amt = nobj * shape->amt * shape->count;
  //fprintf(stderr, "req amt = %ld\n",amt);
  if(amt & 1) ++amt; // round up to even number
  //fprintf(stderr, "rounded req amt = %ld\n",amt);

  // allocate a block
  void *fp = (void *)allocator->allocate(amt);
  assert(fp); // Got some memory!

  if(debug)
    fprintf(stderr,"[gc] Allocated %p, shape=%p = new %s\n", fp,shape,shape->cname);

  Word_t *p = (Word_t*)(void*)JudyLIns(&j_shape,(Word_t)fp,&je);
  *p = ((Word_t)(void*)shape) | (parity & 1);
  if (nobj != 1uL) // array
  {
    Word_t *p = (Word_t*)(void*)JudyLIns(&j_nalloc,(Word_t)fp,&je);
    *p = nobj;
  }

  // update statistics
  allocation_count++;
  allocation_amt += amt;
  //fprintf(stderr,"ADDING %ld to allocation amt, result %ld\n",long(amt),long(allocation_amt));
  // return client memory pointer
  return fp;
}

void flx_collector_t::set_used(void *memory, unsigned long n)
{
  assert(memory);
  assert(n>=0);

  // this check is expensive, but set_used is not used often
  assert(n<=get_count(memory));
  //fprintf(stderr,"Set used of %p to %ld\n",memory,n);
  Word_t *p = (Word_t*)(void*)JudyLGet(j_nused,(Word_t)memory,&je);
  if(p==(Word_t*)PPJERR)judyerror("set_used");
  if(p==NULL)
  {
    //fprintf(stderr,"set_used: No recorded usage! Creating store for data\n");
    p = (Word_t*)(void*)JudyLIns(&j_nused,(Word_t)memory,&je);
  }
  //fprintf(stderr,"Slot for %p usage is address %p\n",memory,p);
  *p = (Word_t)n;
}

void flx_collector_t::incr_used(void *memory, unsigned long n)
{
  assert(memory);
  assert(n>=0);
  //fprintf(stderr,"Incr used of %p by %ld\n",memory,n);
  assert(get_used(memory) + n <= get_count(memory));
  Word_t *p = (Word_t*)(void*)JudyLGet(j_nused,(Word_t)memory,&je);
  if(p==(Word_t*)PPJERR)judyerror("incr_used");
  if(p==NULL)
  {
    //fprintf(stderr,"incr_used: No recorded usage! Creating store for data\n");
    p = (Word_t*)(void*)JudyLIns(&j_nused,(Word_t)memory,&je);
    if(p==(Word_t*)PPJERR)judyerror("incr_used: new slot");
    *p = n;
  }
  else *p+=n;
}

// actual number of used slots in an array
unsigned long flx_collector_t::get_used(void *memory)
{
  assert(memory);
  //fprintf(stderr, "Get used of %p\n",memory);
  Word_t *p = (Word_t*)(void*)JudyLGet(j_nused,(Word_t)memory,&je);
  if(p==(Word_t*)PPJERR)judyerror("get_used");
  //fprintf(stderr, "Used slot at address %p\n",p);
  unsigned long z = p!=NULL?*p:1; // defaults to 1 for non-array support
  //fprintf(stderr,"Used of %p is %ld\n",memory,z);
  return z;
}

// max number of available slots in an array
unsigned long flx_collector_t::get_count(void *memory)
{
  assert(memory);
  //fprintf(stderr, "Get count of %p\n",memory);
  Word_t *p = (Word_t*)(void*)JudyLGet(j_nalloc,(Word_t)memory,&je);
  if(p==(Word_t*)PPJERR)judyerror("get_count");
  //fprintf(stderr, "Count slot at address %p\n",p);
  unsigned long z = p!=NULL?*p:1; // defaults to 1 for non-array support
  //fprintf(stderr,"Count of %p is %ld\n",memory,z);
  return z;
}

gc_shape_t const *flx_collector_t::get_shape(void *memory)
{
  assert(memory);
  //fprintf(stderr, "Get shape of %p\n",memory);
  Word_t *pshape= (Word_t*)JudyLGet(j_shape,(Word_t)memory,&je);
  if(pshape==(Word_t*)PPJERR)judyerror("get_shape");
  if(pshape==NULL) abort();
  return (gc_shape_t const *)(*pshape & (~1ul));
}

void *flx_collector_t::create_empty_array(
  flx::gc::generic::gc_shape_t const *shape,
  unsigned long count
)
{
  //fprintf(stderr,"create empty array length %ld\n",count);
  void *p = allocate(shape,count);
  set_used (p, 0); // make sure to override default 1 slot usage
  //fprintf(stderr,"Array at %p, used = %ld, max=%ld\n",p,get_used(p), get_count(p));
  return p;
}


void flx_collector_t::impl_finalise(void *fp)
{
  assert(fp!=NULL);
  //fprintf(stderr, "Finaliser for %p\n", fp);
  gc_shape_t const *shape = get_shape(fp); // inefficient, since we already know the shape!
  //fprintf(stderr, "Got shape %p=%s\n", shape,shape->cname);
  void (*finaliser)(collector_t*, void*) = shape->finaliser;
  //fprintf(stderr, "Got finaliser %p\n", finaliser);
  if (finaliser)
  {
    unsigned char *cp = (unsigned char*)fp;
    unsigned long n_used = get_count(fp) * shape->count;
    unsigned long eltsize = shape->amt;
    //fprintf(stderr, "Finalising at %p for type %s %ld objects each size %ld\n", cp, shape->cname, n_used, eltsize);
    for(unsigned long j = 0; j<n_used; ++j)
    {
      (*finaliser)(this,(void*)cp);
      cp += eltsize;
    }
  }
}

void flx_collector_t::unlink(void *fp)
{
  // check we have a pointer to an object
  assert(fp!=NULL);

  // call the finaliser if there is one
  //fprintf(stderr,"Calling finaliser\n");
  impl_finalise(fp);

  allocation_count--;
  gc_shape_t const *shape = get_shape(fp);
  unsigned long n_objects = get_count(fp);
  unsigned long nobj = shape -> count * n_objects;
  ::std::size_t size = shape->amt * nobj;
  if (size & 1) ++size;
  //fprintf(stderr, "Uncounting %ld bytes\n", long(size));
  allocation_amt -= size;

  // unlink the frame from the collectors list
  //fprintf(stderr,"Removing address from Judy lists\n");
  JudyLDel(&j_shape, (Word_t)fp, &je);
  JudyLDel(&j_nused, (Word_t)fp, &je);
  JudyLDel(&j_nalloc, (Word_t)fp, &je);
  //fprintf(stderr,"Finished unlinking\n");
}

void flx_collector_t::post_delete(void *fp)
{
  Judy1Set(&j_tmp,(Word_t)fp,&je);
}

void flx_collector_t::delete_frame(void *fp)
{
  allocator->deallocate(fp);
}

unsigned long flx_collector_t::reap ()
{
  unsigned long count = 0;
  Word_t next=(Word_t)NULL;
  int res = Judy1First(j_tmp,&next,&je);
  while(res) {
    delete_frame((void *)(void*)next);
    ++count;
    res = Judy1Next(j_tmp,&next,&je);
  }
  Judy1FreeArray(&j_tmp,&je);
  if(debug) {
    fprintf(stderr,"[gc] Reaped %lu objects\n",count);
    fprintf(stderr,"[gc] Still allocated %lu objects occupying %lu bytes\n", get_allocation_count(), get_allocation_amt());
  }
  return count;
}


//#include <valgrind/memcheck.h>

void flx_collector_t::mark(pthread::memory_ranges_t *px)
{
  int reclimit = 64;
  if(debug)
    fprintf(stderr,"[gc] Collector: Running mark\n");
  assert (root_count == roots.size());
  assert(j_tmp == 0);

  if(px)
  {
    std::vector<pthread::memory_range_t>::iterator end = (*px).end();
    for(
      std::vector<pthread::memory_range_t>::iterator i = (*px).begin();
      i != end;
      ++i
    )
    {
      pthread::memory_range_t range = *i;
      if(debug)
      {
        unsigned long n = (char*)range.e - (char*)range.b;
        fprintf(stderr, "[gc] Conservate scan of memory %p->%p, %ld bytes\n",range.b, range.e, n);
      }
      //VALGRIND_MAKE_MEM_DEFINED(range.b, (char*)range.e-(char*)range.b);
      void *end = range.e;
      for ( void *i = range.b; i != end; i = (void*)((void**)i+1))
      {
        if(debug)
          fprintf(stderr, "[gc] Check if *%p=%p is a pointer\n",i,*(void**)i);
        scan_object(*(void**)i, reclimit);
      }
      if(debug)
        fprintf(stderr, "[gc] DONE: Conservate scan of memory %p->%p\n",range.b, range.e);
    }
  }

  if(debug)
    fprintf(stderr, "[gc] Scanning roots\n");
  rootmap_t::iterator const end = roots.end();
  for(
    rootmap_t::iterator i = roots.begin();
    i != end;
    ++i
  )
  {
    if (debug)
      fprintf(stderr, "[gc] Scanning root %p\n", (*i).first);
    scan_object((*i).first, reclimit);
  }
  // Now, scan the temporary list until it is empty
  Word_t toscan = 0ul;
  int res = Judy1First(j_tmp,&toscan,&je); // get one object scheduled for scanning
  while(res) {
    Judy1Unset(&j_tmp,toscan,&je);         // remove it immediately
    scan_object((void*)toscan, reclimit);            // scan it, it will either be marked or discarded
    toscan = 0ul;
    res = Judy1First(j_tmp,&toscan,&je); 
  }                                     
  assert(j_tmp == 0);                  

  if(debug)
    fprintf(stderr, "[gc] Done Scanning roots\n");
}



unsigned long flx_collector_t::sweep()
{
  if(debug)
    fprintf(stderr,"[gc] Collector: Sweep, garbage bit value=%d\n",(int)parity);
  unsigned long sweeped = 0;
  void *current = NULL;
  Word_t *pshape = (Word_t*)JudyLFirst(j_shape,(Word_t*)&current,&je);
  if(pshape==(Word_t*)PPJERR)judyerror("sweep");

  while(pshape!=NULL)
  {
    if((*pshape & 1) == (parity & 1UL))
    {
      if(debug)
        fprintf(stderr,"[gc] Garbage %p=%s\n",current,((gc_shape_t const *)(*pshape & ~1UL))->cname);
      ++ sweeped;
      //fprintf(stderr,"Unlinking ..\n");
      unlink(current);
      //fprintf(stderr,"Posting delete ..\n");
      post_delete(current);
      //fprintf(stderr,"Reaping done\n");
    }
    else
      if(debug)
        fprintf(stderr,"[gc] Reachable %p=%s\n",current,((gc_shape_t const *)(*pshape & ~1UL))->cname);

    //fprintf(stderr,"Calling Judy for next object\n");
    pshape = (Word_t*)JudyLNext(j_shape,(Word_t*)(void*)&current,&je);
    //fprintf(stderr,"Judy got next object %p\n",pshape);
  }

  parity = !parity;
  if(debug)
    fprintf(stderr,"[gc] Sweeped %ld\n",sweeped);
  return reap();
}

void flx_collector_t::impl_add_root(void *memory)
{
  if(!memory)
  {
    fprintf(stderr, "[gc] GC ERROR: ADD NULL ROOT\n");
    abort();
  }
  rootmap_t::iterator iter = roots.find(memory);
  if(iter == roots.end())
  {
    std::pair<void *const, unsigned long> entry(memory,1UL);
    if(debug) fprintf(stderr,"[gc] Add root %p=%s\n", memory,get_shape(memory)->cname);
    roots.insert (entry);
    root_count++;
  }
  else {
    if(debug) fprintf(stderr,"[gc] Increment root %p to \n", memory, (*iter).second+1);
    ++(*iter).second;
  }
}

void flx_collector_t::impl_remove_root(void *memory)
{
  rootmap_t::iterator iter = roots.find(memory);
  if(iter == roots.end())
  {
    fprintf(stderr, "[gc] GC ERROR: REMOVE ROOT WHICH IS NOT ROOT\n");
    abort();
  }
  if((*iter).second == 1UL)
  {
    if(debug) fprintf(stderr,"[gc] Remove root %p\n", memory);
    roots.erase(iter);
    root_count--;
  }
  else {
    if(debug) fprintf(stderr,"[gc] Decrement root %p to \n", memory, (*iter).second-1);
    --(*iter).second;
  }
}

void flx_collector_t::register_pointer(void *q, int reclimit)
{
  if(reclimit==0)Judy1Set(&j_tmp,(Word_t)q,&je);
  else scan_object(q, reclimit-1);
}

::flx::gc::generic::pointer_data_t flx_collector_t::get_pointer_data (void *p)
{
  ::flx::gc::generic::pointer_data_t pdat;
  pdat.head = NULL;
  pdat.max_elements = 0ul;
  pdat.used_elements = 0ul;
  pdat.shape = NULL;
  pdat.pointer = p;
 
  Word_t cand = (Word_t)p;
  Word_t head = cand;
  Word_t *ppshape = (Word_t*)JudyLLast(j_shape,&head, &je);
  if(ppshape==(Word_t*)PPJERR)judyerror("get_pointer_data");
  if(ppshape == NULL) return pdat; // no lower object
  gc_shape_t const *pshape = (gc_shape_t const *)(*ppshape & ~1UL);
  unsigned long max_slots = get_count((void*)head);
  unsigned long used_slots = get_used((void*)head);
  unsigned long n = max_slots * pshape->count * pshape->amt;
  if(cand >= (Word_t)(void*)((unsigned char*)(void*)head+n)) return pdat; // not interior
  pdat.head = (void*)head;
  pdat.max_elements = max_slots;
  pdat.used_elements = used_slots;
  pdat.shape = pshape;
  return pdat;
}

void flx_collector_t::scan_object(void *p, int reclimit)
{
  Word_t reachable = (parity & 1UL) ^ 1UL;
again:
  if(debug)
    fprintf(stderr,"[gc] Scan object %p, reachable bit value = %d\n",p,(int)reachable);
  Word_t cand = (Word_t)p;
  Word_t head=cand;
  Word_t *ppshape = (Word_t*)JudyLLast(j_shape,&head,&je);
  if(ppshape==(Word_t*)PPJERR)judyerror("scan_object");
  if(ppshape == NULL) return; // no lower object
  /*
  if(debug)
  {
    fprintf(stderr,"Found candidate object %p, &shape=%p, shape(1) %p\n",(void*)fp,(void*)w,(void*)(*w));
    fprintf(stderr," .. type=%s!\n",((gc_shape_t*)(*w & ~1UL))->cname);
  }
  */
  if( (*ppshape & 1UL) == reachable) return;   // already handled

  gc_shape_t const *pshape = (gc_shape_t const *)(*ppshape & ~1UL);
  unsigned long n = get_count((void*)head) * pshape->count * pshape->amt;
  if(cand >= (Word_t)(void*)((unsigned char*)(void*)head+n)) return; // not interior
  if(debug)
    fprintf(stderr,"[gc] MARKING object %p, shape %p, type=%s\n",(void*)head,pshape,pshape->cname);

  *ppshape = (*ppshape & ~1uL) | reachable;


  if(pshape->flags & gc_flags_conservative)
  {
    unsigned long n_used = get_used((void*)head) * pshape->count;
    // end of object, rounded down to size of a void*
    void **end = (void**)(
      (unsigned char*)(void*)head +
      n_used * n / sizeof(void*) * sizeof(void*)
    );
    for ( void **i = (void**)head; i != end; i = i+1)
    {
      //if(debug)
      //  fprintf(stderr, "Check if *%p=%p is a pointer\n",i,*(void**)i);
      if(reclimit==0)
        Judy1Set(&j_tmp,(Word_t)*i,&je);
      else
        scan_object(*i,reclimit -1);
    }
  }
  else
  {
    unsigned long dyncount = get_used((void*)head);
    if(pshape->scanner) {
      void *r = pshape->scanner(this, pshape, (void*)head,dyncount,reclimit);
      if (r) { p = r; goto again; }
    }
  }
}



unsigned long flx_collector_t::impl_collect()
{
  if(debug)
    fprintf(stderr,"[gc] Request to collect, thread %lx\n", (unsigned long)flx::pthread::get_current_native_thread());
  if (thread_control == NULL || thread_control->world_stop())
  {
    if(debug)
      fprintf(stderr,"[gc] Collecting, thread %lx\n", (unsigned long)flx::pthread::get_current_native_thread());
    pthread::memory_ranges_t * mr = thread_control? thread_control -> get_block_list() : NULL;
    mark(mr);
    delete mr;
    unsigned long collected = sweep();
    if(thread_control) thread_control->world_start();
    if(debug)
      fprintf(stderr,"[gc] FINISHED collect, thread %lx\n", (unsigned long)flx::pthread::get_current_native_thread());
    return collected;
  }
  else {
    if(debug)
      fprintf(stderr,"[gc] RACE: someone else is collecting, just yield\n");
    thread_control->yield();
    return 0ul;
  }
}

void flx_collector_t::impl_free_all_mem()
{
  //fprintf(stderr,"impl_free_all_mem -- freeing roots\n");
  roots.clear();
  root_count = 0;
  //fprintf(stderr,"freeing all heap with sweep()\n");
  sweep();
}

flx_collector_t::~flx_collector_t()
{
  //THIS IS VERY DANGEROUS! What if don't want to collect
  //the garbage for efficiency reasons???
  //
  // ELIDED .. already caused a bug!
  //
  //free_all_mem();
}

}}} // end namespaces
