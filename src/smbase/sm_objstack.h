// objstack.h            see license.txt for copyright and terms of use
// stack of objects, owned by the stack

#ifndef OBJSTACK_H
#define OBJSTACK_H

#include "sm_objlist.h"

template <class T>
class ObjStack {
private:      // data
  // will implement the stack as a list, with prepend and removeAt(0)
  ObjList<T> list;

public:       // funcs
  ObjStack()                            : list() {}
  ~ObjStack()                           {}

  int count() const                     { return list.count(); }
  bool isEmpty() const                  { return list.isEmpty(); }
  bool isNotEmpty() const               { return list.isNotEmpty(); }

  T const *topC() const                 { return list.firstC(); }
  T * /*serf*/ top()                    { return list.first(); }

  T * /*owner*/ pop()                   { return list.removeAt(0); }
  void delPop()                         { list.deleteAt(0); }
  void push(T *item)                    { list.prepend(item); }
  void clear()                          { list.deleteAll(); }

  bool contains(T const *item) const    { return list.contains((void*)item); }
};

#endif // OBJSTACK_H
