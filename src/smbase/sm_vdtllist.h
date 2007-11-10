// vdtllist.h            see license.txt for copyright and terms of use
// list of void*, with a pointer maintained to the last (tail)
// element, for constant-time append

#ifndef VDTLLIST_H
#define VDTLLIST_H

#include "sm_voidlist.h"

// inherit privately so I can choose what to expose
class VoidTailList : private VoidList {
private:
  // by making this a friend, it should see VoidList as a
  // base class, and thus simply work
  // but it doesn't..
  //friend VoidListIter;

  friend class VoidTailListIter;

  // no mutator for now

protected:
  VoidNode *tail;       // (serf) last element of list, or NULL if list is empty
  VoidNode *getTop() const { return VoidList::getTop(); }

private:
  VoidTailList(VoidTailList const &obj);    // not allowed

  void adjustTail();

public:
  VoidTailList()                     { tail = NULL; }
  ~VoidTailList()                    {}

  // special ctor which steals the list and then deallocates the header
  VoidTailList(VoidTailList *src)    { tail = NULL; steal(src); }
  void steal(VoidTailList *src);     // deletes 'src'

  // this syntax just makes the implementation inherited from
  // 'VoidList' public, whereas it would default to private,
  // since it was inherited privately
  VoidList::count;

  // see voidlist.h for documentation of each of these functions
  VoidList::isEmpty;
  VoidList::isNotEmpty;
  VoidList::nth;
  VoidList::first;
  void *last() const                 { xassert(tail); return tail->data; }

  // insertion
  void prepend(void *newitem);
  void append(void *newitem);
  void insertAt(void *newitem, int index);
  void concat(VoidTailList &tail);

  // removal
  void *removeFirst();               // remove first, return data; must exist
  void *removeLast();
  void *removeAt(int index);
  void removeAll();
  VoidList::removeItem;

  // list-as-set: selectors
  VoidList::indexOf;
  VoidList::indexOfF;
  VoidList::contains;

  // list-as-set: mutators
  bool prependUnique(void *newitem);
  bool appendUnique(void *newitem);
  //void removeItem(void *item);
  //bool removeIfPresent(void *item);

  // debugging
  void selfCheck() const;
  VoidList::debugPrint;
};


// copied from voidlist.h because g++ won't do what I want..
class VoidTailListIter {
protected:
  VoidNode *p;                        // (serf) current item

public:
  VoidTailListIter(VoidTailList const &list)  { reset(list); }
  ~VoidTailListIter()                         {}

  void reset(VoidTailList const &list)        { p = list.getTop(); }

  // iterator copying; generally safe
  VoidTailListIter(VoidTailListIter const &obj)             { p = obj.p; }
  VoidTailListIter& operator=(VoidTailListIter const &obj)  { p = obj.p; return *this; }

  // but copying from a mutator is less safe; see above
  //VoidTailListIter(VoidListMutator &obj)      { p = obj.current; }

  // iterator actions
  bool isDone() const                         { return p == NULL; }
  void adv()                                  { p = p->next; }
  void *data() const                          { return p->data; }

  // iterator mutation; use with caution
  void setDataLink(void *newData)             { p->data = newData; }
};



#endif // VDTLLIST_H
