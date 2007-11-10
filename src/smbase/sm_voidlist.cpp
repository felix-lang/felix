// voidlist.cc            see license.txt for copyright and terms of use
// code for voidlist.h
#include <functional>

#include "sm_voidlist.h"
#include "sm_breaker.h"
#include "sm_str.h"
#include "sm_ckheap.h"

#include <stdlib.h>     // rand()
#include <stdio.h>      // printf()


VoidList::VoidList(VoidList const &obj)
  : top(NULL)
{
  *this = obj;
}


// # of items in list
int VoidList::count() const
{
  int ct=0;
  for(VoidNode *p = top; p; p = p->next) {
    ct++;
  }
  return ct;
}


// get particular item, 0 is first (item must exist)
void *VoidList::nth(int which) const
{
  VoidNode *p;
  xassert(which>=0);
  for (p = top; which > 0; which--) {
    xassert(p);
    p = p->next;
  }
  if (p == NULL) {
    xfailure(sm_stringc << "asked for list element "
                     << (count()+which) << " (0-based) but list only has "
                     << count() << " elements");
  }
  return p->data;
}


// fail assertion if list fails integrity check
void VoidList::selfCheck() const
{
  if (!top) {
    return;
  }

  // The technique here is the fast/slow list traversal to find loops (which
  // are the only way a singly-linked list can be bad). Basically, if there
  // is a loop then the fast ptr will catch up to and equal the slow one; if
  // not, the fast will simply find the terminating null. It is the only way
  // I know of to find loops in O(1) space and O(n) time.

  VoidNode *slow=top, *fast=top->next;
  while (fast && fast != slow) {
    // check heap properties
    checkHeapNode(fast);

    slow = slow->next;
    fast = fast->next;
    if (fast) {
      checkHeapNode(fast);
      fast = fast->next;      // usually, fast jumps 2 spots per slow's 1
    }
  }

  if (fast == slow) {
    xfailure("linked list has a cycle");
  }
  else {
    return;         // no loop
  }
}


void VoidList::checkHeapDataPtrs() const
{
  for (VoidNode *p=top; p!=NULL; p=p->next) {
    checkHeapNode(p->data);
  }
}


void VoidList::checkUniqueDataPtrs() const
{
  for (VoidNode *p=top; p!=NULL; p=p->next) {
    // run 'q' from 'top' to 'p', looking for any
    // collisions with 'p->data'
    for (VoidNode *q=top; q!=p; q=q->next) {
      if (q->data == p->data) {
        xfailure("linked list with duplicate element");
      }
    }
  }
}


// insert at front
void VoidList::prepend(void *newitem)
{
  top = new VoidNode(newitem, top);
}


// insert at rear
void VoidList::append(void *newitem)
{
  if (!top) {
    prepend(newitem);
  }
  else {
    VoidNode *p;
    for (p = top; p->next; p = p->next)
      {}
    p->next = new VoidNode(newitem);
  }
}


// insert at particular point, index of new node becomes 'index'
void VoidList::insertAt(void *newitem, int index)
{
  if (index == 0 || isEmpty()) {
    // special case prepending or an empty list
    xassert(index == 0);     // if it's empty, index should be 0
    prepend(newitem);
  }

  else {
    // Looking at the loop below, the key things to note are:
    //  1. If index started as 1, the loop isn't executed, and the new
    //     node goes directly after the top node.
    //  2. p is never allowed to become NULL, so we can't walk off the
    //     end of the list.

    index--;
    VoidNode *p;
    for (p = top; p->next && index; p = p->next) {
      index--;
    }
    xassert(index == 0);
      // if index isn't 0, then index was greater than count()

    // put a node after p
    VoidNode *n = new VoidNode(newitem);
    n->next = p->next;
    p->next = n;
  }
}


void VoidList::insertSorted(void *newitem, VoidDiff diff, void *extra)
{
  // put it first?
  if (!top ||
      diff(newitem, top->data, extra) <= 0) {                // newitem <= top
    prepend(newitem);
    return;
  }

  // we will be considering adding 'newitem' *after* cursor, as we go
  VoidNode *cursor = top;
  while (cursor->next != NULL &&
         diff(cursor->next->data, newitem, extra) < 0) {     // cursor->next < newitem
    cursor = cursor->next;
  }

  // insert 'newitem' after 'cursor'
  VoidNode *newNode = new VoidNode(newitem);
  newNode->next = cursor->next;
  cursor->next = newNode;
}


// ----------------- list-as-set stuff -------------------
// get the index of an item's first occurrance
int VoidList::indexOf(void *item) const
{
  int index = 0;
  for (VoidNode *p = top; p != NULL; p = p->next, index++) {
    if (p->data == item) {
      return index;
    }
  }
  return -1;
}


int VoidList::indexOfF(void *item) const
{
  int ret = indexOf(item);
  xassert(ret >= 0);
  return ret;
}


bool VoidList::prependUnique(void *newitem)
{
  if (!contains(newitem)) {
    prepend(newitem);
    return true;
  }
  else {
    return false;   // no change
  }
}


bool VoidList::appendUnique(void *newitem)
{
  if (!top) {
    prepend(newitem);
    return true;
  }

  // walk to the end of list, while checking to
  // see if 'newitem' is already in the list
  VoidNode *p;
  for (p = top; p->next; p = p->next) {
    if (p->data == newitem) {
      return false;      // item is already on list; no change
    }
  }
  if (p->data == newitem) {
    return false;
  }

  p->next = new VoidNode(newitem);
  return true;
}


bool VoidList::removeIfPresent(void *item)
{
  // for now, not a real fast implementation
  int index = indexOf(item);
  if (index == -1) {
    return false;   // not there
  }
  else {
    removeAt(index);
    return true;
  }
}


void VoidList::removeItem(void *item)
{
  bool wasThere = removeIfPresent(item);
  xassert(wasThere);
}

// ----------------- end of list-as-set stuff -------------------


void *VoidList::removeAt(int index)
{
  if (index == 0) {
    xassert(top != NULL);   // element must exist to remove
    VoidNode *temp = top;
    void *retval = temp->data;
    top = top->next;
    delete temp;
    return retval;
  }

  // will look for the node just before the one to delete
  index--;

  VoidNode *p;
  for (p = top; p->next && index>0;
       p = p->next, index--)
    {}

  if (p->next) {
    // index==0, so p->next is node to remove
    VoidNode *temp = p->next;
    void *retval = temp->data;
    p->next = p->next->next;
    delete temp;
    return retval;
  }
  else {
    // p->next==NULL, so index was too large
    xfailure("Tried to remove an element not on the list");
    return NULL;    // silence warning
  }
}


void VoidList::removeAll()
{
  while (top != NULL) {
    VoidNode *temp = top;
    top = top->next;
    delete temp;
  }
}


void VoidList::reverse()
{
  // transfer list to a temporary
  VoidNode *oldlist = top;
  top = NULL;

  // prepend all nodes
  while (oldlist != NULL) {
    // take first node from oldlist
    VoidNode *node = oldlist;
    oldlist = oldlist->next;

    // prepend it to new list
    node->next = top;
    top = node;
  }
}


//   The difference function should return <0 if left should come before
// right, 0 if they are equivalent, and >0 if right should come before
// left. For example, if we are sorting numbers into ascending order,
// then 'diff' would simply be subtraction.
//   The 'extra' parameter passed to sort is passed to diff each time it
// is called.
//   O(n^2) time, O(1) space
void VoidList::insertionSort(VoidDiff diff, void *extra)
{
  VoidNode *primary = top;                   // primary sorting pointer
  while (primary && primary->next) {
    if (diff(primary->data, primary->next->data, extra) > 0) {   // must move next node?
      VoidNode *tomove = primary->next;
      primary->next = primary->next->next;    // patch around moving node

      if (diff(tomove->data, top->data, extra) < 0) {           // new node goes at top?
        tomove->next = top;
        top = tomove;
      }

      else {                                  // new node *not* at top
        VoidNode *searcher = top;
        while (diff(tomove->data, searcher->next->data, extra) > 0) {
          searcher = searcher->next;
        }

        tomove->next = searcher->next;        // insert new node into list
        searcher->next = tomove;
      }
    }

    else {
      primary = primary->next;              // go on if no changes
    }
  }
}


// O(n log n) time, O(log n) space
void VoidList::mergeSort(VoidDiff diff, void *extra)
{
  if (top == NULL || top->next == NULL) {
    return;   // base case: 0 or 1 elements, already sorted
  }

  // half-lists
  VoidList leftHalf;
  VoidList rightHalf;

  // divide the list
  {
    // to find the halfway point, we use the slow/fast
    // technique; to get the right node for short lists
    // (like 2-4 nodes), we start fast off one ahead

    VoidNode *slow = top;
    VoidNode *fast = top->next;

    while (fast && fast->next) {
      slow = slow->next;
      fast = fast->next;
      fast = fast->next;
    }

    // at this point, 'slow' points to the node
    // we want to be the last of the 'left' half;
    // the left half will either be the same length
    // as the right half, or one node longer

    // division itself
    rightHalf.top = slow->next;  // top half to right
    leftHalf.top = this->top;    // bottom half to left
    slow->next = NULL;           // cut the link between the halves
  }

  // recursively sort the halves
  leftHalf.mergeSort(diff, extra);
  rightHalf.mergeSort(diff, extra);

  // merge the halves into a single, sorted list
  VoidNode *merged = NULL;       // tail of merged list
  while (leftHalf.top != NULL &&
         rightHalf.top != NULL) {
    // see which first element to select, and remove it
    VoidNode *selected;
    if (diff(leftHalf.top->data, rightHalf.top->data, extra) < 0) {
      selected = leftHalf.top;
      leftHalf.top = leftHalf.top->next;
    }
    else {
      selected = rightHalf.top;
      rightHalf.top = rightHalf.top->next;
    }

    // append it to the merged list
    if (merged == NULL) {
      // first time; special case
      merged = this->top = selected;
    }
    else {
      // 2nd and later; normal case
      merged = merged->next = selected;
    }
  }

  // one of the halves is exhausted; concat the
  // remaining elements of the other one
  if (leftHalf.top != NULL) {
    merged->next = leftHalf.top;
    leftHalf.top = NULL;
  }
  else {
    merged->next = rightHalf.top;
    rightHalf.top = NULL;
  }

  // verify both halves are now exhausted
  xassert(leftHalf.top == NULL &&
          rightHalf.top == NULL);

  // list is sorted
}


bool VoidList::isSorted(VoidDiff diff, void *extra) const
{
  if (isEmpty()) {
    return true;
  }

  void *prev = top->data;
  VoidListIter iter(*this);
  iter.adv();
  for (; !iter.isDone(); iter.adv()) {
    void *current = iter.data();

    if (diff(prev, current, extra) <= 0) {
      // ok: prev <= current
    }
    else {
      return false;
    }

    prev = current;
  }

  return true;
}


// attach tail's nodes to this; empty the tail
void VoidList::concat(VoidList &tail)
{
  if (!top) {
    top = tail.top;
  }
  else {
    VoidNode *n = top;
    for(; n->next; n = n->next)
      {}
    n->next = tail.top;
  }

  tail.top = NULL;
}


// attach some of source's nodes to this, removing them from 'source'
void VoidList::stealTailAt(int index, VoidList &source)
{
  if (index == 0) {
    concat(source);
    return;
  }

  // find the node in 'source' just before the first one that
  // will be transferred
  VoidNode *beforeTransfer = source.top;
  index--;
  while (index--) {
    beforeTransfer = beforeTransfer->next;
  }

  // break off the tail
  VoidNode *tailStart = beforeTransfer->next;
  beforeTransfer->next = NULL;

  // transfer 'tailStart' and beyond to 'this'
  if (!top) {
    top = tailStart;
  }
  else {
    // find the end of 'this' list
    VoidNode *n = top;
    for(; n->next; n = n->next)
      {}
    n->next = tailStart;
  }
}


void VoidList::appendAll(VoidList const &tail)
{
  // make a dest iter and move it to the end
  VoidListMutator destIter(*this);
  while (!destIter.isDone()) {
    destIter.adv();
  }

  VoidListIter srcIter(tail);
  for (; !srcIter.isDone(); srcIter.adv()) {
    destIter.append(srcIter.data());
  }
}


VoidList& VoidList::operator= (VoidList const &src)
{
  if (this != &src) {
    removeAll();
    appendAll(src);
  }
  return *this;
}


bool VoidList::equalAsLists(VoidList const &otherList, VoidDiff diff, void *extra) const
{
  return 0==compareAsLists(otherList, diff, extra);
}

int VoidList::compareAsLists(VoidList const &otherList, VoidDiff diff, void *extra) const
{
  VoidListIter mine(*this);
  VoidListIter his(otherList);

  while (!mine.isDone() && !his.isDone()) {
    int cmp = diff(mine.data(), his.data(), extra);
    if (cmp == 0) {
      // they are equal; keep going
    }
    else {
      // unequal, return which way comparison went
      return cmp;
    }

    mine.adv();
    his.adv();
  }

  if (!mine.isDone() || !his.isDone()) {
    // unequal lengths: shorter compares as less
    return mine.isDone()? -1 : +1;
  }

  return 0;        // everything matches
}


bool VoidList::equalAsSets(VoidList const &otherList, VoidDiff diff, void *extra) const
{
  return this->isSubsetOf(otherList, diff, extra) &&
         otherList.isSubsetOf(*this, diff, extra);
}


bool VoidList::isSubsetOf(VoidList const &otherList, VoidDiff diff, void *extra) const
{
  for (VoidListIter iter(*this); !iter.isDone(); iter.adv()) {
    if (!otherList.containsByDiff(iter.data(), diff, extra)) {
      return false;
    }
  }
  return true;
}


bool VoidList::containsByDiff(void *item, VoidDiff diff, void *extra) const
{
  for (VoidListIter iter(*this); !iter.isDone(); iter.adv()) {
    if (0==diff(item, iter.data(), extra)) {
      return true;
    }
  }
  return false;
}


STATICDEF int VoidList::pointerAddressDiff(void *left, void *right, void*)
{
  // originally, left - right which is +ve if right < left
  return
    std::less<void*>()(right, left) ? 1 :
    ((left == right) ? 0 : -1);
}


void VoidList::debugPrint() const
{
  printf("{ ");
  for (VoidListIter iter(*this); !iter.isDone(); iter.adv()) {
    printf("%p ", iter.data());
  }
  printf("}");
}


// --------------- VoidListMutator ------------------
VoidListMutator&
  VoidListMutator::operator=(VoidListMutator const &obj)
{
  // we require that the underlying lists be the same
  // because we can't reset the 'list' reference if they
  // aren't
  xassert(&list == &obj.list);

  prev = obj.prev;
  current = obj.current;

  return *this;
}


void VoidListMutator::insertBefore(void *item)
{
  if (prev == NULL) {
    // insert at start of list
    list.prepend(item);
    reset();
  }
  else {
    current = prev->next = new VoidNode(item, current);
  }
}


void VoidListMutator::insertAfter(void *item)
{
  xassert(!isDone());
  current->next = new VoidNode(item, current->next);
}


void VoidListMutator::append(void *item)
{
  xassert(isDone());
  insertBefore(item);
  adv();
}


void *VoidListMutator::remove()
{
  xassert(!isDone());
  void *retval = data();
  if (prev == NULL) {
    // removing first node
    list.top = current->next;
    delete current;
    current = list.top;
  }
  else {
    current = current->next;
    delete prev->next;   // old 'current'
    prev->next = current;
  }
  return retval;
}


// --------------- VoidListIter --------------------
VoidListIter::VoidListIter(VoidList const &list, int pos)
{
  reset(list);
  while (pos--) {
    adv();
  }
}


// -------------- testing --------------
#ifdef TEST_VOIDLIST
#include "sm_test.h"

// assumes we're using pointerAddressDiff as the comparison fn
// (I don't use isSorted because this fn will throw at the disequality,
// whereas isSorted would forget that info)
void verifySorted(VoidList const &list)
{
  int prev = 0;
  VoidListIter iter(list);
  for (; !iter.isDone(); iter.adv()) {
    int current = (int)iter.data();
    xassert(prev <= current);    // numeric address test
    prev = current;
  }
}


#define PRINT(lst) printf("%s: ", #lst); lst.debugPrint(); printf("\n") /* user ; */

void testSorting()
{
  enum { ITERS=100, ITEMS=20 };

  loopi(ITERS) {
    // construct a list (and do it again if it ends up already sorted)
    VoidList list1;
    VoidList list3;     // this one will be constructed sorted one at a time
    int numItems;
    do {
      list1.removeAll();    // clear them in case we have to build it more than once
      list3.removeAll();
      numItems = rand()%ITEMS;
      loopj(numItems) {
        void *toInsert = (void*)( (rand()%ITEMS) * 4 );
        list1.prepend(toInsert);
        list3.insertSorted(toInsert, VoidList::pointerAddressDiff);
      }
    } while (list1.isSorted(VoidList::pointerAddressDiff));

    // list3 should be sorted already
    //PRINT(list3);
    verifySorted(list3);

    // duplicate it for use with other algorithm
    VoidList list2;
    list2 = list1;

    // sort them
    list1.insertionSort(VoidList::pointerAddressDiff);
    xassert(list1.equalAsPointerSets(list2));      // verify intermediate equality
    xassert(!list1.equalAsPointerLists(list2));    // but not elementwise
    list2.mergeSort(VoidList::pointerAddressDiff);
    //PRINT(list1);

    // verify structure
    list1.selfCheck();
    list2.selfCheck();

    // verify length
    xassert(list1.count() == numItems && list2.count() == numItems);

    // verify sortedness
    verifySorted(list1);
    verifySorted(list2);

    // verify equality
    xassert(list1.equalAsPointerLists(list2));
    xassert(list1.equalAsPointerLists(list3));

    // to test as-sets equality
    void *first = list1.first();
    while (list1.removeIfPresent(first))
      {}     // remove all occurrances of 'first'
    xassert(!list1.equalAsPointerSets(list2));
  }
}


void entry()
{
  // first set of tests
  {
    // some sample items
    void *a=(void*)4, *b=(void*)8, *c=(void*)12, *d=(void*)16;

    VoidList list;

    // test simple modifiers and info
    list.append(c);     PRINT(list);   // c
    list.prepend(b);    PRINT(list);   // b c
    list.append(d);     PRINT(list);   // b c d
    list.prepend(a);    PRINT(list);   // a b c d
    list.removeAt(2);   PRINT(list);   // a b d

    xassert( list.count() == 3 &&
             !list.isEmpty() &&
             list.nth(0) == a &&
             list.nth(1) == b &&
             list.nth(2) == d &&
             list.indexOf(a) == 0 &&
             list.indexOf(b) == 1 &&
             list.indexOf(c) == -1 &&
             list.indexOf(d) == 2
           );
    list.selfCheck();

    // test mutator s
    {
      VoidListMutator mut(list);
      mut.adv();
        // now it's pointing at b
      mut.insertAfter(c);
        // now list is (a b c d) and mut points at b still
      verifySorted(list);
      mut.remove();
        // now list is (a c d) and mut points at c
      xassert(mut.data() == c);

      // copy the mutator
      VoidListMutator mut2(mut);
      mut2.adv();
      xassert(mut.data() == c  &&  mut2.data() == d);

      // copy to a normal iterator
      VoidListIter iter(mut);
      iter.adv();
      xassert(iter.data() == d);
      iter.adv();
      xassert(iter.isDone()  &&  mut.data() == c);

      PRINT(list);
    }

    // test appendUnique and prependUnique
    // list starts as (a c d)
    xassert(list.appendUnique(c) == false &&
            list.prependUnique(d) == false &&
            list.prependUnique(b) == true);
      // now it is (b a c d)
    list.removeItem(a);
    xassert(list.removeIfPresent(a) == false);
      // now it is (b c d)
    verifySorted(list);
    PRINT(list);

    // test reverse
    list.reverse();
      // list is now (d c b)
    xassert(list.indexOf(d) == 0 &&
            list.indexOf(c) == 1 &&
            list.indexOf(b) == 2);
    PRINT(list);

    // test stealTailAt
    VoidList thief;
    thief.stealTailAt(1, list);
      // list is now (d)
      // thief is now (c b)
    xassert(list.count() == 1 &&
            list.indexOf(d) == 0 &&
            thief.count() == 2 &&
            thief.indexOf(c) == 0 &&
            thief.indexOf(b) == 1);
  }

  // this hits most of the remaining code
  // (a decent code coverage tool for C++ would be nice!)
  testSorting();

  printf("voidlist ok\n");
}

USUAL_MAIN

#endif // NDEBUG

