// sobjstack.h            see license.txt for copyright and terms of use
// stack of objects, *not* owned by the stack

#ifndef SOBJSTACK_H
#define SOBJSTACK_H

#include "sm_sobjlist.h"

template <class T>
class SObjStack {
public:       // data
  // will implement the stack as a list, with prepend and removeAt(0)
  SObjList<T> list;

public:       // funcs
  SObjStack()                           : list() {}
  ~SObjStack()                          {}

  int count() const                     { return list.count(); }
  bool isEmpty() const                  { return list.isEmpty(); }
  bool isNotEmpty() const               { return list.isNotEmpty(); }

  T const *topC() const                 { return list.firstC(); }
  T *top()                              { return list.first(); }

  T *pop()                              { return list.removeAt(0); }
  void push(T *item)                    { list.prepend(item); }

  bool contains(T const *item) const    { return list.contains((void*)item); }
};


// utility class for maintaining a first-class sub-stack of the AST
// stack isomorphic to the stackframe stack; Note that the fact that
// nothing happens if 'obj' is NULL is a feature: sometimes you can't
// map the need to call this class completely onto the control flow,
// and so some dataflow is involved; since the dtor for this class is
// used as a kind of finally statement, we can't nest its construction
// in an 'if' statement!  Instead pass in NULL if you want a no-op
// effect.
template<class T>
class StackMaintainer {
  SObjStack<T> &s;
  T *obj;

  StackMaintainer(StackMaintainer&); // forbid copying

public:
  explicit StackMaintainer(SObjStack<T> &s0, T *obj0)
    : s(s0)
    , obj(obj0)
  {
    if (obj) {
      s.push(obj);
    }
  }

  ~StackMaintainer() {
    if (obj) {
      T *obj0 = s.pop();
      xassert(obj0 == obj);
    }
  }
};


#endif // SOBJSTACK_H
