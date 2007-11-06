#include "pthread_sleep_queue.hpp"
#include <queue>        // stl to the bloated rescue
#include <string.h>       // strerror

using namespace std;

namespace flx { namespace pthread {
typedef queue<void*> void_queue;

#define ELTQ ((void_queue*)lame_opaque)

sleep_queue_t::sleep_queue_t(size_t n) : bound(n)
{
  lame_opaque = new void_queue;
}

// Much care is needed deleting a queue.
// A safe method is possible .. but not provided here
sleep_queue_t::~sleep_queue_t()
{
  delete ELTQ;
}

void sleep_queue_t::wait_until_empty() {
  flx_mutex_locker_t   l(member_lock);
  while(!ELTQ->empty())
    size_changed.wait(&member_lock);
}

void
sleep_queue_t::enqueue(void* elt)
{
  flx_mutex_locker_t   l(member_lock);
  while(ELTQ->size() >= bound) // guard against spurious wakeups!
    size_changed.wait(&member_lock);
  ELTQ->push(elt);
  size_changed.broadcast(); // cannot return an error
}

void*
sleep_queue_t::dequeue()
{
  flx_mutex_locker_t   l(member_lock);
  while(ELTQ->empty())  // guard against spurious wakeups!
    size_changed.wait(&member_lock);
  void *elt = ELTQ->front();
  ELTQ->pop();
  size_changed.broadcast();
  return elt;
}

void
sleep_queue_t::resize(size_t n)
{
  flx_mutex_locker_t   l(member_lock);
  bound = n;
  // get things rolling again
  size_changed.broadcast();
}

}}


