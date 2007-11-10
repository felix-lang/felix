// sobjlist.h
// serf list of arbitrary objects
// NOTE: automatically generated from xobjlist.h -- do not edit directly

// Author: Scott McPeak, 2000

#ifndef SOBJLIST_H
#define SOBJLIST_H

#include "sm_voidlist.h"


// forward declarations of template classes, so we can befriend them in SObjList
// (not required by Borland C++ 4.5, but GNU wants it...)
template <class T> class SObjListIter;
template <class T> class SObjListMutator;
template <class T> class SObjListIterNC;


// the list is considered to not own any of the items; it's ok to
// insert items multiple times or into multiple lists
template <class T>
class SObjList {
private:
  friend class SObjListIter<T>;
  friend class SObjListMutator<T>;
  friend class SObjListIterNC<T>;

protected:
  VoidList list;                        // list itself

public:
  // make shallow copies
  SObjList(SObjList const &obj)         : list(obj.list) {}
  SObjList& operator= (SObjList const &src)         { list = src.list; return *this; }

public:
  SObjList()                            : list() {}
  ~SObjList()                           {}    /* all items removed */

  // The difference function should return <0 if left should come before
  // right, 0 if they are equivalent, and >0 if right should come before
  // left.  For example, if we are sorting numbers into ascending order,
  // then 'diff' would simply be subtraction.
  typedef int (*Diff)(T const *left, T const *right, void *extra);

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
  void prepend(T *newitem)              { list.prepend((void*)newitem); }
  void append(T *newitem)               { list.append((void*)newitem); }
  void insertAt(T *newitem, int index)  { list.insertAt((void*)newitem, index); }
  void insertSorted(T *newitem, Diff diff, void *extra=NULL)
    { list.insertSorted((void*)newitem, (VoidDiff)diff, extra); }

  // removal
  T *removeAt(int index)                { return (T*)list.removeAt(index); }
  T *removeFirst()                      { return (T*)list.removeFirst(); }
  void removeAll()                      { list.removeAll(); }

  // list-as-set: selectors
  int indexOf(T const *item) const      { return list.indexOf((void*)item); }
  int indexOfF(void *item) const        { return list.indexOfF((void*)item); }
  bool contains(T const *item) const    { return list.contains((void*)item); }

  // list-as-set: mutators
  bool prependUnique(T *newitem)        { return list.prependUnique((void*)newitem); }
  bool appendUnique(T *newitem)         { return list.appendUnique((void*)newitem); }
  void removeItem(T const *item)        { list.removeItem((void*)item); }    // whether the arg should be const is debatable..
  bool removeIfPresent(T const *item)   { return list.removeIfPresent((void*)item); }

  // complex modifiers
  void reverse()                                    { list.reverse(); }
  void insertionSort(Diff diff, void *extra=NULL)   { list.insertionSort((VoidDiff)diff, extra); }
  void mergeSort(Diff diff, void *extra=NULL)       { list.mergeSort((VoidDiff)diff, extra); }

  // and a related test
  bool isSorted(Diff diff, void *extra=NULL) const  { return list.isSorted((VoidDiff)diff, extra); }

  // multiple lists
  void concat(SObjList &tail)                       { list.concat(tail.list); }
  void appendAll(SObjList const &tail)              { list.appendAll(tail.list); }

  // steal
  void stealTailAt(int index, SObjList &tail)       { list.stealTailAt(index, tail.list); }

  // equal items in equal positions
  bool equalAsLists(SObjList const &otherList, Diff diff, void *extra=NULL) const
    { return list.equalAsLists(otherList.list, (VoidDiff)diff, extra); }
  int compareAsLists(SObjList const &otherList, Diff diff, void *extra=NULL) const
    { return list.compareAsLists(otherList.list, (VoidDiff)diff, extra); }

  // last-as-set: comparisons (NOT efficient)
  bool equalAsSets(SObjList const &otherList, Diff diff, void *extra=NULL) const
    { return list.equalAsSets(otherList.list, (VoidDiff)diff, extra); }
  bool isSubsetOf(SObjList const &otherList, Diff diff, void *extra=NULL) const
    { return list.isSubsetOf(otherList.list, (VoidDiff)diff, extra); }
  bool containsByDiff(T const *item, Diff diff, void *extra=NULL) const
    { return list.containsByDiff((void*)item, (VoidDiff)diff, extra); }

  // treating the pointer values themselves as the basis for comparison
  bool equalAsPointerLists(SObjList const &otherList) const
    { return list.equalAsPointerLists(otherList.list); }
  bool equalAsPointerSets(SObjList const &otherList) const
    { return list.equalAsPointerSets(otherList.list); }

  // debugging: no invariants beyond VoidList
  void selfCheck() const                { list.selfCheck(); }

  // but export the additional checks for cases where they apply anyway
  void checkHeapDataPtrs() const        { list.checkHeapDataPtrs(); }
  void checkUniqueDataPtrs() const      { list.checkUniqueDataPtrs(); }
};


// for traversing the list and modifying it (nodes and/or structure)
// NOTE: no list-modification fns should be called on 'list' while this
//       iterator exists, and only one such iterator should exist for
//       any given list
template <class T>
class SObjListMutator {
  friend class SObjListIter<T>;

protected:
  VoidListMutator mut;       // underlying mutator

public:
  SObjListMutator(SObjList<T> &lst)     : mut(lst.list) { reset(); }
  ~SObjListMutator()                    {}

  void reset()                          { mut.reset(); }

  // iterator copying; safe *only* until one of the mutators modifies
  // the list structure (by inserting or removing), at which time all
  // other iterators might be in limbo
  SObjListMutator(SObjListMutator const &obj)             : mut(obj.mut) {}
  SObjListMutator& operator=(SObjListMutator const &obj)  { mut = obj.mut;  return *this; }
    // requires that 'this' and 'obj' already refer to the same 'list'

  // iterator actions
  bool isDone() const                   { return mut.isDone(); }
  void adv()                            { mut.adv(); }
  T *data()                             { return (T*)mut.data(); }
  T *&dataRef()                         { return (T*&)mut.dataRef(); }

  // insertion
  void insertBefore(T *item)            { mut.insertBefore((void*)item); }
    // 'item' becomes the new 'current', and the current 'current' is
    // pushed forward (so the next adv() will make it current again)

  void insertAfter(T *item)             { mut.insertAfter((void*)item); }
    // 'item' becomes what we reach with the next adv();
    // isDone() must be false

  void append(T *item)                  { mut.append((void*)item); }
    // only valid while isDone() is true, it inserts 'item' at the end of
    // the list, and advances such that isDone() remains true; equivalent
    // to { xassert(isDone()); insertBefore(item); adv(); }

  // removal
  T *remove()                           { return (T*)mut.remove(); }
    // 'current' is removed from the list and returned, and whatever was
    // next becomes the new 'current'

  // debugging
  void selfCheck() const                { mut.selfCheck(); }
};

#define SMUTATE_EACH_OBJLIST(T, list, iter) \
  for(SObjListMutator< T > iter(list); !iter.isDone(); iter.adv())


// for traversing the list without modifying it (neither nodes nor structure)
// NOTE: no list-modification fns should be called on 'list' while this
//       iterator exists
template <class T>
class SObjListIter {
protected:
  VoidListIter iter;      // underlying iterator

public:
  SObjListIter(SObjList<T> const &list) : iter(list.list) {}
  SObjListIter(SObjList<T> const &list, int pos) : iter(list.list, pos) {}
  ~SObjListIter()                       {}

  void reset(SObjList<T> const &list)   { iter.reset(list.list); }

  // iterator copying; generally safe
  SObjListIter(SObjListIter const &obj)             : iter(obj.iter) {}
  SObjListIter& operator=(SObjListIter const &obj)  { iter = obj.iter;  return *this; }

  // but copying from a mutator is less safe; see above
  SObjListIter(SObjListMutator<T> &obj)             : iter(obj.mut) {}

  // iterator actions
  bool isDone() const                   { return iter.isDone(); }
  void adv()                            { iter.adv(); }
  T const *data() const                 { return (T const*)iter.data(); }
};

#define SFOREACH_OBJLIST(T, list, iter) \
  for(SObjListIter< T > iter(list); !iter.isDone(); iter.adv())


// intermediate to the above two, this allows modification of the
// objects stored on the list, but not the identity or order of
// the objects in the list
template <class T>
class SObjListIterNC {
protected:
  VoidListIter iter;      // underlying iterator

public:
  SObjListIterNC(SObjList<T> &list) : iter(list.list) {}
  SObjListIterNC(SObjList<T> &list, int pos) : iter(list.list, pos) {}
  ~SObjListIterNC()                     {}

  void reset(SObjList<T> &list)         { iter.reset(list.list); }

  // iterator copying; generally safe
  SObjListIterNC(SObjListIterNC const &obj)             : iter(obj.iter) {}
  SObjListIterNC& operator=(SObjListIterNC const &obj)  { iter = obj.iter;  return *this; }

  // but copying from a mutator is less safe; see above
  SObjListIterNC(SObjListMutator<T> &obj)               : iter(obj.mut) {}

  // iterator actions
  bool isDone() const                   { return iter.isDone(); }
  void adv()                            { iter.adv(); }
  T *data() const                       { return (T*)iter.data(); }
};

#define SFOREACH_OBJLIST_NC(T, list, iter) \
  for(SObjListIterNC< T > iter(list); !iter.isDone(); iter.adv())


// iterate over the combined elements of two or more lists
template <class T>
class SObjListMultiIter {
private:
  // all the lists
  SObjList<T> **lists;               // serf array of serf list pointers
  int numLists;                      // length of this array

  // current element
  int curList;                       // which list we're working on
  SObjListIter<T> iter;              // current element of that list

  // invariant:
  //   either curList==numLists, or
  //   iter is not 'done'

public:
  SObjListMultiIter(SObjList<T> **L, int n)
    : lists(L),
      numLists(n),
      curList(0),
      iter(*(lists[0]))
  {
    xassert(n > 0);
    normalize();
  }

  // advance the iterator to the next element of the next non-empty list;
  // establishes invariant above
  void normalize();

  bool isDone() const {
    return curList == numLists;
  }

  T const *data() const {
    return iter.data();
  }

  void adv() {
    iter.adv();
    normalize();
  }
};

// this was originally inline, but that was causing some strange
// problems (compiler bug?)
template <class T>
void SObjListMultiIter<T>::normalize()
{
  while (iter.isDone() && curList < numLists) {
    curList++;
    if (curList < numLists) {
      iter.reset(*(lists[curList]));
    }
  }
}


#endif // SOBJLIST_H
