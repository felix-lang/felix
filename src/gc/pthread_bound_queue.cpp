#include "pthread_bound_queue.hpp"
#include <queue>        // stl to the bloated rescue
#include <stdio.h>      // debugging in scanner

using namespace std;

namespace flx { namespace pthread {
typedef deque<void*> void_queue;

#define ELTQ ((void_queue*)lame_opaque)

bound_queue_t::bound_queue_t(size_t n) : bound(n)
{
  lame_opaque = new void_queue;
}

// Much care is needed deleting a queue.
// A safe method is possible .. but not provided here
bound_queue_t::~bound_queue_t()
{
  delete ELTQ;
}

// get the number of element in the queue
// (NOT the bound!)
size_t bound_queue_t::len() {
  flx_mutex_locker_t   l(member_lock);
  return ELTQ->size();
}

void bound_queue_t::wait_until_empty() {
  flx_mutex_locker_t   l(member_lock);
  while(!ELTQ->empty())
    size_changed.wait(&member_lock);
}

void
bound_queue_t::enqueue(void* elt)
{
  flx_mutex_locker_t   l(member_lock);
  while(ELTQ->size() >= bound) // guard against spurious wakeups!
    size_changed.wait(&member_lock);
  ELTQ->push_back(elt);
  size_changed.broadcast(); // cannot return an error
}

void*
bound_queue_t::dequeue()
{
  flx_mutex_locker_t   l(member_lock);
  while(ELTQ->empty())  // guard against spurious wakeups!
    size_changed.wait(&member_lock);
  void *elt = ELTQ->front();
  ELTQ->pop_front();
  size_changed.broadcast();
  return elt;
}

void*
bound_queue_t::maybe_dequeue()
{
  flx_mutex_locker_t   l(member_lock);
  void *elt = NULL;
  if (ELTQ->size() > 0)
  {
    elt = ELTQ->front();
    ELTQ->pop_front();
    size_changed.broadcast();
  }
  return elt;
}


void
bound_queue_t::resize(size_t n)
{
  flx_mutex_locker_t   l(member_lock);
  bound = n;
  // get things rolling again
  size_changed.broadcast();
}

using namespace flx;;
using namespace gc;
using namespace generic;

void *bound_queue_scanner(
  collector_t *collector, 
  gc_shape_t *shape, void *pp, 
  unsigned long dyncount, 
  int reclimit
)
{
  // input is a pointer to a pointer to a bound queue object
  void *p = *(void**)pp;
  bound_queue_t *bq = (bound_queue_t*)p;
  void_queue *pq = (void_queue*) bq->lame_opaque;
  printf("Scanning bound queue %p->%p\n", pp, p);
  
  ::std::deque<void*>::const_iterator stl_end = pq->end();
  for(
    ::std::deque<void*>::const_iterator iter= pq->begin(); 
    iter < stl_end;
    ++iter
  ) {
    void *value = *iter;
    printf("bound_queue scanning p=%p\n",value); 
    collector->register_pointer(value,reclimit);
  }
  return 0;
}


}}


