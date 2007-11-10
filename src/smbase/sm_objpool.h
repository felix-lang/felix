// objpool.h            see license.txt for copyright and terms of use
// custom allocator: array of objects meant to be
// re-used frequently, with high locality

#ifndef OBJPOOL_H
#define OBJPOOL_H

#include "sm_array.h"

// the class T should have:
//   // a link in the free list; it is ok for T to re-use this
//   // member while the object is not free in the pool
//   T *nextInFreeList;
//
//   // object is done being used for now
//   void deinit();
//
//   // needed so we can make arrays
//   T::T();

template <class T>
class ObjectPool {
private:     // data
  // when the pool needs to expand, it expands by allocating an
  // additional 'rackSize' objects; I use a linear (instead of
  // exponential) expansion strategy because these are supposed
  // to be used for small sets of rapidly-reused objects, not
  // things allocated for long-term storage
  int rackSize;

  // growable array of pointers to arrays of 'rackSize' T objects
  ArrayStack<T*> racks;

  // head of the free list; NULL when empty
  T *head;

private:     // funcs
  void expandPool();

public:      // funcs
  ObjectPool(int rackSize);
  ~ObjectPool();

  // yields a pointer to an object ready to be used; typically,
  // T should have some kind of init method to play the role a
  // constructor ordinarily does; this might cause the pool to
  // expand (but previously allocated objects do *not* move)
  inline T *alloc();

  // return an object to the pool of objects; dealloc internally
  // calls obj->deinit()
  inline void dealloc(T *obj);

  // same as 'dealloc', but without the call to 'deinit'
  inline void deallocNoDeinit(T *obj);

  // available for diagnostic purposes
  int freeObjectsInPool() const;

  // low-level access for heavily-optimized client code; clients that
  // use these functions accept the burden of possibly needing to
  // change if internals of ObjectPool change
  T *private_getHead() { return head; }
  void private_setHead(T *h) { head = h; }
};


template <class T>
ObjectPool<T>::ObjectPool(int rs)
  : rackSize(rs),
    racks(5),
    head(NULL)
{}

template <class T>
ObjectPool<T>::~ObjectPool()
{
  // deallocate all the objects in the racks
  for (int i=0; i < racks.length(); i++) {
    delete[] racks[i];
  }
}


template <class T>
inline T *ObjectPool<T>::alloc()
{
  if (!head) {
    // need to expand the pool
    expandPool();
  }

  T *ret = head;                     // prepare to return this one
  head = ret->nextInFreeList;        // move to next free node

  #ifndef NDEBUG
    ret->nextInFreeList = NULL;        // paranoia
  #endif

  return ret;
}


// this is pulled out of 'alloc' so alloc can be inlined
// without causing excessive object code bloat
template <class T>
void ObjectPool<T>::expandPool()
{
  T *rack = new T[rackSize];
  racks.push(rack);

  // thread new nodes into a free list
  for (int i=rackSize-1; i>=0; i--) {
    rack[i].nextInFreeList = head;
    head = &(rack[i]);
  }
}


// usually I want the convenience of dealloc calling deinit; however,
// in the inner loop of a performance-critical section of code, I
// want finer control
template <class T>
inline void ObjectPool<T>::deallocNoDeinit(T *obj)
{
  // I don't check that nextInFreeList == NULL, despite having set it
  // that way in alloc(), because I want to allow for users to make
  // nextInFreeList share storage (e.g. with a union) with some other
  // field that gets used while the node is allocated

  // prepend the object to the free list; will be next yielded
  obj->nextInFreeList = head;
  head = obj;
}


template <class T>
inline void ObjectPool<T>::dealloc(T *obj)
{
  // call obj's pseudo-dtor (the decision to have dealloc do this is
  // motivated by not wanting to have to remember to call deinit
  // before dealloc)
  obj->deinit();

  deallocNoDeinit(obj);
}


template <class T>
int ObjectPool<T>::freeObjectsInPool() const
{
  T *p = head;
  int ct = 0;

  while (p) {
    ct++;
    p = p->nextInFreeList;
  }

  return ct;
}


#endif // OBJPOOL_H
