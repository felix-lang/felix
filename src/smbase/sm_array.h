// array.h            see license.txt for copyright and terms of use
// some array classes

#ifndef ARRAY_H
#define ARRAY_H

#include "sm_xassert.h"


// -------------------- Array ----------------------
// This is the same as C++'s built-in array, but automatically deallocates.
// If you want bounds checking too, use GrowArray, below.
template <class T>
class Array {
private:     // data
  T *arr;

private:     // not allowed
  Array(Array&);
  void operator=(Array&);

public:
  Array(int len) : arr(new T[len]) {}
  ~Array() { delete[] arr; }

  T const &operator[] (int i) const { return arr[i]; }
  T &operator[] (int i) { return arr[i]; }

  operator T const* () const { return arr; }
  operator T const* () { return arr; }
  operator T * () { return arr; }

  T const* operator+ (int i) const { return arr+i; }
  T * operator+ (int i) { return arr+i; }

  // convenience
  void setAll(T val, int len) {
    for (int i=0; i<len; i++) {
      arr[i] = val;
    }
  }
};


// ------------------ GrowArray --------------------
// This class implements an array of T's; it automatically expands
// when 'ensureAtLeast' or 'ensureIndexDoubler' is used; it does not
// automatically contract.  All accesses are bounds-checked.
//
// class T must have:
//   T::T();           // default ctor for making arrays
//   operator=(T&);    // assignment for copying to new storage
//   T::~T();          // dtor for when old array is cleared
template <class T>
class GrowArray {
private:     // data
  T *arr;                 // underlying array; NULL if sz==0
  int sz;                 // # allocated entries in 'arr'

private:     // funcs
  void bc(int i) const    // bounds-check an index
    { xassert((unsigned)i < (unsigned)sz); }
  void eidLoop(int index);

public:      // funcs
  GrowArray(int initSz);
  ~GrowArray();

  // allocated space
  int size() const { return sz; }

  // element access
  T const& operator[] (int i) const   { bc(i); return arr[i]; }
  T      & operator[] (int i)         { bc(i); return arr[i]; }

  // set size, reallocating if old size is different; if the
  // array gets bigger, existing elements are preserved; if the
  // array gets smaller, elements are truncated
  void setSize(int newSz);

  // make sure there are at least 'minSz' elements in the array;
  void ensureAtLeast(int minSz)
    { if (minSz > sz) { setSize(minSz); } }

  // grab a read-only pointer to the raw array
  T const *getArray() const { return arr; }

  // grab a writable pointer; use with care
  T *getDangerousWritableArray() { return arr; }
  T *getArrayNC() { return arr; }     // ok, not all that dangerous..

  // make sure the given index is valid; if this requires growing,
  // do so by doubling the size of the array (repeatedly, if
  // necessary)
  void ensureIndexDoubler(int index)
    { if (sz-1 < index) { eidLoop(index); } }

  // set an element, using the doubler if necessary
  void setIndexDoubler(int index, T const &value)
    { ensureIndexDoubler(index); arr[index] = value; }

  // swap my data with the data in another GrowArray object
  void swapWith(GrowArray<T> &obj) {
    T *tmp1 = obj.arr; obj.arr = this->arr; this->arr = tmp1;
    int tmp2 = obj.sz; obj.sz = this->sz; this->sz = tmp2;
  }

  // convenience
  void setAll(T val) {
    for (int i=0; i<sz; i++) {
      arr[i] = val;
    }
  }
};


template <class T>
GrowArray<T>::GrowArray(int initSz)
{
  sz = initSz;
  if (sz > 0) {
    arr = new T[sz];
  }
  else {
    arr = NULL;
  }
}


template <class T>
GrowArray<T>::~GrowArray()
{
  if (arr) {
    delete[] arr;
  }
}


template <class T>
void GrowArray<T>::setSize(int newSz)
{
  if (newSz != sz) {
    // keep track of old
    int oldSz = sz;
    T *oldArr = arr;

    // make new
    sz = newSz;
    if (sz > 0) {
      arr = new T[sz];
    }
    else {
      arr = NULL;
    }

    // copy elements in common
    for (int i=0; i<sz && i<oldSz; i++) {
      arr[i] = oldArr[i];
    }

    // get rid of old
    if (oldArr) {
      delete[] oldArr;
    }
  }
}


// this used to be ensureIndexDoubler's implementation, but
// I wanted the very first check to be inlined
template <class T>
void GrowArray<T>::eidLoop(int index)
{
  if (sz-1 >= index) {
    return;
  }

  int newSz = sz;
  while (newSz-1 < index) {
    #ifndef NDEBUG_NO_ASSERTIONS    // silence warning..
      int prevSz = newSz;
    #endif
    if (newSz == 0) {
      newSz = 1;
    }
    newSz = newSz*2;
    xassert(newSz > prevSz);        // otherwise overflow -> infinite loop
  }

  setSize(newSz);
}


// ---------------------- ArrayStack ---------------------
// This is an array where some of the array is unused.  Specifically,
// it maintains a 'length', and elements 0 up to length-1 are
// considered used, whereas length up to size-1 are unused.  The
// expected use is as a stack, where "push" adds a new (used) element.
template <class T>
class ArrayStack : public GrowArray<T> {
private:
  int len;               // # of elts in the stack

public:
  ArrayStack(int initArraySize = 10)
    : GrowArray<T>(initArraySize),
      len(0)
    {}
  ~ArrayStack();

  // element access; these declarations are necessary because
  // the uses of 'operator[]' below are not "dependent", hence
  // they can't use declarations inherited from GrowArray<T>
  T const& operator[] (int i) const { return GrowArray<T>::operator[](i); }
  T      & operator[] (int i)       { return GrowArray<T>::operator[](i); }

  void push(T const &val)
    { setIndexDoubler(len++, val); }
  T pop()
    { return operator[](--len); }
  T const &top() const
    { return operator[](len-1); }
  T &top()
    { return operator[](len-1); }

  // alternate interface, where init/deinit is done explicitly
  // on returned references
  T &pushAlt()    // returns newly accessible item
    { GrowArray<T>::ensureIndexDoubler(len++); return top(); }
  T &popAlt()     // returns item popped
    { return operator[](--len); }

  // items stored
  int length() const
    { return len; }

  bool isEmpty() const
    { return len==0; }
  bool isNotEmpty() const
    { return !isEmpty(); }

  void popMany(int ct)
    { len -= ct; xassert(len >= 0); }
  void empty()
    { len = 0; }

  // useful when someone has used 'getDangerousWritableArray' to
  // fill the array's internal storage
  void setLength(int L) { len = L; }

  // consolidate allocated space to match length
  void consolidate() { setSize(length()); }

  // swap
  void swapWith(ArrayStack<T> &obj) {
    GrowArray<T>::swapWith(obj);
    int tmp = obj.len; obj.len = this->len; this->len = tmp;
  }
};

template <class T>
ArrayStack<T>::~ArrayStack()
{}


// iterator over contents of an ArrayStack, to make it easier to
// switch between it and SObjList as a representation
template <class T>
class ArrayStackIterNC {
  NO_OBJECT_COPIES(ArrayStackIterNC);   // for now

private:     // data
  ArrayStack<T> /*const*/ &arr;   // array being accessed
  int index;                      // current element

public:      // funcs
  ArrayStackIterNC(ArrayStack<T> /*const*/ &a) : arr(a), index(0) {}

  // iterator actions
  bool isDone() const             { return index >= arr.length(); }
  void adv()                      { xassert(!isDone()); index++; }
  T /*const*/ *data() const       { return &(arr[index]); }
};


// I want const polymorphism!


// pop (and discard) a value off a stack at end of scope
template <class T>
class ArrayStackPopper {
private:
  ArrayStack<T> &stk;

public:
  ArrayStackPopper(ArrayStack<T> &s) : stk(s) {}
  ArrayStackPopper(ArrayStack<T> &s, T const &pushVal)
    : stk(s) { stk.push(pushVal); }
  ~ArrayStackPopper()
    { stk.pop(); }
};


// ------------------- ObjArrayStack -----------------
// an ArrayStack of owner pointers
template <class T>
class ObjArrayStack {
private:    // data
  ArrayStack<T*> arr;

public:     // funcs
  ObjArrayStack(int initArraySize = 10)
    : arr(initArraySize)
    {}
  ~ObjArrayStack() { deleteAll(); }

  void push(T *ptr)          { arr.push(ptr); }
  T *pop()                   { return arr.pop(); }

  T const *topC() const      { return arr.top(); }
  T       *top()             { return arr.top(); }

  T const * operator[](int index) const  { return arr[index]; }
  T *       operator[](int index)        { return arr[index]; }

  int length() const         { return arr.length(); }
  bool isEmpty() const       { return arr.isEmpty(); }
  bool isNotEmpty() const    { return !isEmpty(); }

  void deleteTopSeveral(int ct);
  void deleteAll()           { deleteTopSeveral(length()); }

  // will not delete any items
  void consolidate()         { arr.consolidate(); }

  void swapWith(ObjArrayStack<T> &obj)   { arr.swapWith(obj.arr); }
};


template <class T>
void ObjArrayStack<T>::deleteTopSeveral(int ct)
{
  while (ct--) {
    delete pop();
  }
}


// ------------------------- ArrayStackEmbed --------------------------
// This is like ArrayStack, but the first 'n' elements are stored
// embedded in this object, instead of allocated on the heap; in some
// circumstances, this lets us avoid allocating memory in common cases.
//
// For example, suppose you have an algorithm that is usually given a
// small number of elements, say 1 or 2, but occasionally needs to
// work with more.  If you put the array of elements in the heap, then
// even in the common case a heap allocation is required, which is
// bad.  But by using ArrayStackEmbed<T,2>, you can be sure that if
// the number of elements is <= 2 there will be no heap allocation,
// even though you still get a uniform (array-like) interface to all
// the elements.
template <class T, int n>
class ArrayStackEmbed {
private:      // data
  // embedded storage
  T embed[n];

  // heap-allocated storage
  GrowArray<T> heap;

  // total number of elements in the stack; if this
  // exceeds 'n', then heap.arr is non-NULL
  int len;

private:      // funcs
  void bc(int i) const    // bounds-check an index
    { xassert((unsigned)i < (unsigned)len); }

public:       // funcs
  ArrayStackEmbed()
    : /*embed is default-init'd*/
      heap(0),    // initially a NULL ptr
      len(0)
  {}
  ~ArrayStackEmbed()
  {}              // heap auto-deallocs its internal data

  void push(T const &val)
  {
    if (len < n) {
      embed[len++] = val;
    }
    else {
      heap.setIndexDoubler(len++ - n, val);
    }
  }

  T pop()
  {
    xassert(len > 0);
    if (len <= n) {
      return embed[--len];
    }
    else {
      return heap[--len - n];
    }
  }

  int length() const
    { return len; }
  bool isEmpty() const
    { return len==0; }
  bool isNotEmpty() const
    { return !isEmpty(); }

  // direct element access
  T const &getElt(int i) const
  {
    bc(i);
    if (i < n) {
      return embed[i];
    }
    else {
      return heap[i - n];
    }
  }

  T const& operator[] (int i) const
    { return getElt(i); }
  T & operator[] (int i)
    { return const_cast<T&>(getElt(i)); }

  T const &top() const
    { return getElt(len-1); }
};


#endif // ARRAY_H
