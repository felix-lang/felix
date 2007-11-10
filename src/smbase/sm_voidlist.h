// voidlist.h            see license.txt for copyright and terms of use
// list of void*

// Author: Scott McPeak, 2000

#ifndef __VOIDLIST_H
#define __VOIDLIST_H

#include "sm_xassert.h"
#include "sm_typ.h"
#include "sm_trdelete.h"

// -------------------------- non-typesafe core -----------------------------
// non-typesafe list node
class VoidNode {
public:
  TRASHINGDELETE

  VoidNode *next;           // (owner) next item in list, or NULL if last item
  void *data;               // whatever it is the list is holding

  VoidNode(void *aData=NULL, VoidNode *aNext=NULL) { data=aData; next=aNext; }
};


// forward decls for 'friend' decls
class VoidListIter;
class VoidListMutator;


// The difference function should return <0 if left should come before
// right, 0 if they are equivalent, and >0 if right should come before
// left.  For example, if we are sorting numbers into ascending order,
// then 'diff' could simply be subtraction.
typedef int (*VoidDiff)(void *left, void *right, void *extra);


// list of void*; at this level, the void* are completely opaque;
// the list won't attempt to delete(), compare them, or anything else
// (well, some comparison has creeped in now... but only via VoidDiff)
class VoidList {
private:
  friend class VoidListIter;
  friend class VoidListMutator;

protected:
  VoidNode *top;                     // (owner) first node, or NULL if list is empty
  VoidNode *getTop() const { return top; } // for iterator, below

public:
  VoidList()                         { top=NULL; }
  VoidList(VoidList const &obj);     // makes a (shallow) copy of the contents
  ~VoidList()                        { removeAll(); }

  // selectors
  int count() const;                 // # of items in list
  bool isEmpty() const               { return top == NULL; }
  bool isNotEmpty() const            { return top != NULL; }
  void *nth(int which) const;        // get particular item, 0 is first (item must exist)
  void *first() const { return nth(0); }
  void *last() const { return nth(count()-1); }

  // insertion
  void prepend(void *newitem);       // insert at front
  void append(void *newitem);        // insert at rear
  void insertAt(void *newitem, int index);
    // new item is inserted such that its index becomdes 'index'
  void insertSorted(void *newitem, VoidDiff diff, void *extra=NULL);
    // insert into an already-sorted list so that the list is sorted afterwards

  // removal
  void *removeAt(int index);         // remove from list (must exist), and return removed item
  void *removeFirst()                { return removeAt(0); }
  void removeAll();

  // list-as-set: selectors
  int indexOf(void *item) const;     // returns index of *first* occurrance, or -1 if not present
  int indexOfF(void *item) const;    // same as indexOf, but throws exception if not present
  bool contains(void *item) const    // true if the item appears in the list
    { return indexOf(item) >= 0; }

  // list-as-set: mutators
  bool prependUnique(void *newitem); // prepend only if not already there
  bool appendUnique(void *newitem);  // append   "            "
  void removeItem(void *item);       // remove first occurrance -- must exist
  bool removeIfPresent(void *item);  // remove first occurrance; return true if changed

  // complex modifiers
  void reverse();
  void insertionSort(VoidDiff diff, void *extra=NULL);
  void mergeSort(VoidDiff diff, void *extra=NULL);

  // and a related test
  bool isSorted(VoidDiff diff, void *extra=NULL) const;

  // multiple lists
  void concat(VoidList &tail);           // tail is emptied, nodes appended to this
  void appendAll(VoidList const &tail);  // tail is untouched.. but its contents are now exposed to non-constness... ug... oh well
  VoidList& operator= (VoidList const &src);  // afterwards, 'this' and 'src' have same contents

  // steal (become the container for) the tail of a source list at any
  // point; if 'index' is 0, the entire 'source' is stolen (i.e.
  // index=0 is equivalent to 'concat', above); stolen items appended
  // to 'this'
  void stealTailAt(int index, VoidList &source);

  // equal items in equal positions
  bool equalAsLists(VoidList const &otherList, VoidDiff diff, void *extra=NULL) const;

  // if equal, returns 0; otherwise, return order (-1/+1) according to
  // the first differing pair of elements; a shorter (but otherwise
  // idential list) will compare as being less
  int compareAsLists(VoidList const &otherList, VoidDiff diff, void *extra=NULL) const;

  // last-as-set: comparisons (NOT efficient)
  bool equalAsSets(VoidList const &otherList, VoidDiff diff, void *extra=NULL) const;
    // A subset of B, and vice-versa
  bool isSubsetOf(VoidList const &otherList, VoidDiff diff, void *extra=NULL) const;
    // uses slow elementwise containment
  bool containsByDiff(void *item, VoidDiff diff, void *extra=NULL) const;

  // treating the pointer values themselves as the basis for comparison
  static int pointerAddressDiff(void *left, void *right, void*);
  bool equalAsPointerLists(VoidList const &otherList) const
    { return equalAsLists(otherList, pointerAddressDiff); }
  bool equalAsPointerSets(VoidList const &otherList) const
    { return equalAsSets(otherList, pointerAddressDiff); }

  // debugging
  void selfCheck() const;            // test this list; fail assertion if malformed
  void debugPrint() const;           // print list contents to stdout
  void checkHeapDataPtrs() const;    // fail assertion if any 'data' ptr isn't valid heap ptr
  void checkUniqueDataPtrs() const;  // fail assertion if any 'data' ptr matches any other in this list
};


// for traversing the list and modifying it
// NOTE: no list-modification fns should be called on 'list' while this
//       iterator exists, and only one such iterator should exist for
//       any given list
class VoidListMutator {
  friend class VoidListIter;

protected:
  VoidList &list;         // underlying list
  VoidNode *prev;         // (serf) previous node; NULL if at list's head
  VoidNode *current;      // (serf) node we're considered to be pointing at

public:
  VoidListMutator(VoidList &lst)   : list(lst) { reset(); }
  ~VoidListMutator()               {}

  void reset()                     { prev = NULL;  current = list.top; }

  // iterator copying; safe *only* until one of the mutators modifies
  // the list structure (by inserting or removing), at which time all
  // other iterators might be in limbo
  VoidListMutator(VoidListMutator const &obj)
    : list(obj.list), prev(obj.prev), current(obj.current) {}
  VoidListMutator& operator=(VoidListMutator const &obj);
    // requires that 'this' and 'obj' already refer to the same 'list'

  // iterator actions
  bool isDone() const              { return current == NULL; }
  void adv()                       { prev = current;  current = current->next; }
  void *data()                     { return current->data; }
  void *&dataRef()                 { return current->data; }

  // insertion
  void insertBefore(void *item);
    // 'item' becomes the new 'current', and the current 'current' is
    // pushed forward (so the next adv() will make it current again)

  void insertAfter(void *item);
    // 'item' becomes what we reach with the next adv();
    // isDone() must be false

  void append(void *item);
    // only valid while isDone() is true, it inserts 'item' at the end of
    // the list, and advances such that isDone() remains true; equivalent
    // to { xassert(isDone()); insertBefore(item); adv(); }

  // removal
  void *remove();
    // 'current' is removed from the list and returned, and whatever was
    // next becomes the new 'current'

  // debugging
  void selfCheck() const
    { xassert((prev->next == current  &&  current != list.top) ||
              (prev==NULL && current==list.top)); }
};


// for traversing the list without modifying it
// NOTE: no list-modification fns should be called on 'list' while this
//       iterator exists
class VoidListIter {
protected:
  VoidNode *p;                        // (serf) current item

public:
  VoidListIter(VoidList const &list)  { reset(list); }
  VoidListIter(VoidList const &list, int pos);    // advance 'pos' times
  ~VoidListIter()                     {}

  void reset(VoidList const &list)    { p = list.getTop(); }

  // iterator copying; generally safe
  VoidListIter(VoidListIter const &obj)             { p = obj.p; }
  VoidListIter& operator=(VoidListIter const &obj)  { p = obj.p;  return *this; }

  // but copying from a mutator is less safe; see above
  VoidListIter(VoidListMutator &obj)  { p = obj.current; }

  // iterator actions
  bool isDone() const                 { return p == NULL; }
  void adv()                          { p = p->next; }
  void *data() const                  { return p->data; }
};


#endif // __VOIDLIST_H
