// owner.h            see license.txt for copyright and terms of use
// a stab at an owner ptr abstraction

#ifndef OWNER_H
#define OWNER_H

#include "sm_typ.h"

#ifdef DEBUG_OWNER
  #include <stdio.h>    // printf, temporary
  #define DBG(fn) printf("%s(%p)\n", fn, ptr)
#else
  #define DBG(fn)
#endif

template <class T>
class Owner {
private:    // data
  T *ptr;                // the real pointer

private:    // funcs
  Owner(Owner&);         // not allowed

public:     // funcs
  Owner(T *p = NULL) : ptr(p) { DBG("ctor"); }
  ~Owner() { DBG("dtor"); del(); }

  // take ownership (no transitive = here)
  void operator= (T *p) { DBG("op=ptr"); del(); ptr=p; }
  void operator= (Owner<T> &obj) { DBG("op=obj"); del(); ptr=obj.ptr; obj.ptr=NULL; }

  // release ownership
  T *xfr() { DBG("xfr"); T *temp = ptr; ptr = NULL; return temp; }

  // free
  void del() { DBG("del"); delete ptr; ptr = NULL; }    // relies on delete(NULL) being ok

  // some operators that make Owner behave more or less like
  // a native C++ pointer.. note that some compilers to really
  // bad handling the "ambiguity", so the non-const versions
  // can be disabled at compile time
  operator T const* () const { DBG("opcT*"); return ptr; }
  T const & operator* () const { DBG("opc*"); return *ptr; }
  T const * operator-> () const { DBG("opc->"); return ptr; }

  // according to http://www.google.com/search?q=cache:zCRFFDMZvVUC:people.we.mediaone.net/stanlipp/converops.htm+conversion+sequence+for+the+argument+is+better&hl=en&ie=ISO-8859-1,
  // a solution to the gcc "conversion sequence is better" complaint
  // is to define this version
  operator T const* () { DBG("opcT*_nc"); return ptr; }

  #ifndef NO_OWNER_NONCONST
  operator T* () { DBG("opT*"); return ptr; }
  T& operator* () { DBG("op*"); return *ptr; }
  T* operator-> () { DBG("op->"); return ptr; }
  #endif

  // escape hatch for when operators flake out on us
  T *get() { DBG("get"); return ptr; }
  T const *getC() const { DBG("getC"); return ptr; }

  // even more dangerous escape; only use where the caller
  // agrees to restore the owner invariant!
  T *&getRef() { DBG("getRed"); return ptr; }

  // swaps are interesting because they don't require checking
  void swapWith(Owner<T> &obj) {
    T *tmp = ptr;
    ptr = obj.ptr;
    obj.ptr = tmp;
  }
};


template <class T>
void swap(Owner<T> &obj1, Owner<T> &obj2)
{
  obj1.swapWith(obj2);
}


// not used with Owner objects, but rather with
// simple pointers (Foo*) that are used as owners
template <class T>
T *xfr(T *&ptr)
{
  T *ret = ptr;
  ptr = NULL;
  return ret;
}


#endif // OWNER_H
