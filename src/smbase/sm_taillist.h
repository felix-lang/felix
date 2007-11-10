// taillist.h; see license.txt for copyright and terms of use
// list wrapper around VoidTailList in the spirit of ASTList, but doesn't own

// taken almost verbatim from asttlist.h in smbase

#ifndef TAILLIST_H
#define TAILLIST_H

#include "sm_vdtllist.h"

template <class T> class TailListIter;
template <class T> class TailListIterNC;

// a list which does not own the items in it (will NOT deallocate
// them), and has constant-time access to the last element
template <class T>
class TailList {
private:
  friend class TailListIter<T>;
  friend class TailListIterNC<T>;

protected:
  VoidTailList list;            // list itself

private:
  TailList(TailList const &obj); // not allowed

public:
  TailList()                             : list() {}
  ~TailList()                            {  }

  // ctor to make singleton list; often quite useful
  TailList(T *elt)                       : list() { prepend(elt); }

  // stealing ctor; among other things, since &src->list is assumed to
  // point at 'src', this class can't have virtual functions;
  // these ctors delete 'src'
  TailList(TailList<T> *src)              : list(&src->list) {}
  void steal(TailList<T> *src)           { list.steal(&src->list); }

  // selectors
  int count() const                     { return list.count(); }
  bool isEmpty() const                  { return list.isEmpty(); }
  bool isNotEmpty() const               { return list.isNotEmpty(); }
  T *nth(int which)                     { return (T*)list.nth(which); }
  T const *nthC(int which) const        { return (T const*)list.nth(which); }
  T *first()                            { return (T*)list.first(); }
  T const *firstC() const               { return (T const*)list.first(); }
  T *last()                             { return (T*)list.last(); }
  T const *lastC() const                { return (T const*)list.last(); }

  // insertion
  void prepend(T *newitem)              { list.prepend(newitem); }
  void append(T *newitem)               { list.append(newitem); }
  void insertAt(T *newitem, int index)  { list.insertAt(newitem, index); }
  void concat(TailList<T> &tail)         { list.concat(tail.list); }

  // removal
  T *removeFirst()                      { return (T*)list.removeFirst(); }
  T *removeLast()                       { return (T*)list.removeLast(); }
  T *removeAt(int index)                { return (T*)list.removeAt(index); }
  void removeItem(T *item)              { list.removeItem((void*)item); }

  // list-as-set: selectors
  int indexOf(T const *item) const      { return list.indexOf((void*)item); }
  int indexOfF(T const *item) const     { return list.indexOfF((void*)item); }
  bool contains(T const *item) const    { return list.contains((void*)item); }

  // list-as-set: mutators
  bool prependUnique(T *newitem)        { return list.prependUnique(newitem); }
  bool appendUnique(T *newitem)         { return list.appendUnique(newitem); }

  // debugging: two additional invariants
  void selfCheck() const                { list.selfCheck(); }
};


template <class T>
class TailListIter {
protected:
  VoidTailListIter iter;        // underlying iterator

public:
  TailListIter(TailList<T> const &list) : iter(list.list) {}
  ~TailListIter()                       {}

  void reset(TailList<T> const &list)   { iter.reset(list.list); }

  // iterator copying; generally safe
  TailListIter(TailListIter const &obj)             : iter(obj.iter) {}
  TailListIter& operator=(TailListIter const &obj)  { iter = obj.iter;  return *this; }

  // iterator actions
  bool isDone() const                   { return iter.isDone(); }
  void adv()                            { iter.adv(); }
  T const *data() const                 { return (T const*)iter.data(); }
};

#define FOREACH_TAILLIST(T, list, iter) \
  for(TailListIter<T> iter(list); !iter.isDone(); iter.adv())


// version of the above, but for non-const-element traversal
template <class T>
class TailListIterNC {
protected:
  VoidTailListIter iter;        // underlying iterator

public:
  TailListIterNC(TailList<T> &list)      : iter(list.list) {}
  ~TailListIterNC()                     {}

  void reset(TailList<T> &list)         { iter.reset(list.list); }

  // iterator copying; generally safe
  TailListIterNC(TailListIterNC const &obj)             : iter(obj.iter) {}
  TailListIterNC& operator=(TailListIterNC const &obj)  { iter = obj.iter;  return *this; }

  // iterator actions
  bool isDone() const                   { return iter.isDone(); }
  void adv()                            { iter.adv(); }
  T *data() const                       { return (T*)iter.data(); }

  // iterator mutation; use with caution
  void setDataLink(T *newData)          { iter.setDataLink((void*)newData); }
};

#define FOREACH_TAILLIST_NC(T, list, iter) \
  for(TailListIterNC<T> iter(list); !iter.isDone(); iter.adv())

#endif // TailLIST_H
