// vdtllist.cc            see license.txt for copyright and terms of use
// code for vdtllist.h

#include "sm_vdtllist.h"

void VoidTailList::steal(VoidTailList *src)
{
  if (src) {
    top = src->top;
    tail = src->tail;
    src->top = NULL;    // paranoia
    delete src;
  }
  else {
    top = NULL;
    tail = NULL;
  }
}

void VoidTailList::prepend(void *newitem)
{
  VoidList::prepend(newitem);
  if (!tail) {
    tail = top;
  }
}

void VoidTailList::append(void *newitem)
{
  if (isEmpty()) {
    prepend(newitem);
  }
  else {
    // payoff: constant-time append
    tail->next = new VoidNode(newitem, NULL);
    tail = tail->next;
  }
}

void VoidTailList::insertAt(void *newitem, int index)
{
  VoidList::insertAt(newitem, index);
  adjustTail();
}

void VoidTailList::concat(VoidTailList &srcList)
{
  // grab what will be the tail of the concatenated list
  VoidNode *catTail = srcList.top? srcList.tail : tail;

  // build proper backbone structure
  VoidList::concat(srcList);

  // fix tails
  tail = catTail;
  srcList.tail = NULL;
}

void VoidTailList::adjustTail()
{
  if (!tail) {
    tail = top;
  }
  else if (tail->next) {
    tail = tail->next;
  }
  xassert(tail->next == NULL);
}

void *VoidTailList::removeFirst()
{
  xassert(top);
  if (top == tail) {
    tail = NULL;
  }
  void *retval = top->data;
  VoidNode *tmp = top;
  top = top->next;
  delete tmp;
  return retval;
}

void *VoidTailList::removeLast()
{
  xassert(top);
  if (top == tail) {
    return removeFirst();
  }

  VoidNode *before = top;
  while (before->next != tail) {
    before = before->next;
  }
  void *retval = tail->data;
  delete tail;
  tail = before;
  tail->next = NULL;
  return retval;
}

void *VoidTailList::removeAt(int index)
{
  xassert(top);
  if (index == 0) {
    return removeFirst();
  }

  VoidNode *before = top;    // will point to node before one to be removed
  index--;
  while (index > 0) {
    before = before->next;
    index--;
  }
  xassert(index == 0);

  // fix 'tail' if necessary
  if (tail == before->next) {
    tail = before;
  }

  // patch around before->next
  VoidNode *toDelete = before->next;
  void *retval = toDelete->data;
  before->next = toDelete->next;
  delete toDelete;

  return retval;
}

void VoidTailList::removeAll()
{
  VoidList::removeAll();
  tail = NULL;
}

bool VoidTailList::prependUnique(void *newitem)
{
  bool retval = VoidList::prependUnique(newitem);
  adjustTail();
  return retval;
}

bool VoidTailList::appendUnique(void *newitem)
{
  bool retval = VoidList::appendUnique(newitem);
  adjustTail();
  return retval;
}

void VoidTailList::selfCheck() const
{
  VoidList::selfCheck();

  if (isNotEmpty()) {
    // get last node
    VoidNode *n = top;
    while (n->next) {
      n = n->next;
    }

    // 'tail' should be the last one
    xassert(tail == n);
  }
  else {
    xassert(tail == NULL);
  }
}


// --------------------- test code ------------------
#ifdef TEST_VDTLLIST

#include <stdio.h>    // printf

int main()
{
  VoidTailList list;
  int zero, one, two, three;

  // This isn't a very exhaustive test; it's mainly to check that
  // selfCheck doesn't do anything really stupid (it used to).

  list.selfCheck();

  list.append(&two);     list.selfCheck();
  list.prepend(&one);    list.selfCheck();
  list.append(&three);   list.selfCheck();
  list.prepend(&zero);   list.selfCheck();

  xassert(list.nth(0) == &zero);
  xassert(list.nth(1) == &one);
  xassert(list.nth(2) == &two);
  xassert(list.nth(3) == &three);

  list.removeAll();
  list.selfCheck();

  printf("vdtllist works\n");

  return 0;
}

#endif // TEST_VDTLLIST
