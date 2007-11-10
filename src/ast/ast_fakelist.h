// fakelist.h            see license.txt for copyright and terms of use
// headerless list of nodes where each node has a 'next' field

#ifndef FAKELIST_H
#define FAKELIST_H

// idea: define a templatized class such that a pointer to
// this class appears to present access to a list of the
// underlying T objects; but in fact, the pointer is actually
// a T pointer, and T contains a field called 'next' which
// inductively defines the list contents.  then such a pointer
// could be used to provide documentation of the presence of
// the list (and not just one node), and a uniform (with other
// list interfaces) syntactic interface, while hiding an
// efficient representation

// why not just insist that all objects be derived from some
// base class, e.g. FakeListNode, that defines 'next'?  because
// what is the type of 'next'?  it has to be FakeListNode*, but
// then either it must be physically first, or else 'next' is
// a pointer to the interior and I have to worry about whether
// the casts to/from the outer type will be correctly offset; and
// then getting to the next node requires a cast (the usual
// problem with subtyping polymorphism)

// for now, the list is non-owning (unless you call 'deallocNodes')

class Some_undefined_class;

template <class T>
class FakeList {
private:
  // you can't create or delete one of these
  FakeList();
  ~FakeList();

  // silence a silly egcs-1.1.2 warning (this function isn't defined)
  friend class Some_undefined_class;

  // this class has *no* data--an object of this type is
  // never actually created!  instead we play with pointers
  // to this "type", and cast to T* as necessary

public:
  // this is as much of a constructor as there is
  static FakeList<T> *makeList(T *node) { return (FakeList<T>*)node; }
  static FakeList<T> *emptyList()       { return NULL; }

  // this will deallocate all the nodes in the list; the list itself
  // is, therefore, also deallocated and should not be used after this
  void deallocNodes();

  // simple selectors
  int count() const;
  bool isEmpty() const                  { return this == NULL; }
  bool isNotEmpty() const               { return !isEmpty(); }

  // "car" in Lisp terminology
  T *first()                            { return (T*)this; }
  T const *firstC() const               { return (T const*)this; }

  // "cdr" in Lisp terminology
  FakeList<T> *butFirst()               { return makeList(first()->next); }
  FakeList<T> const *butFirstC() const  { return makeList(firstC()->next); }

  // similar to "cons" in Lisp terminology (but this doesn't allocate)
  FakeList<T> *prepend(T *newHead)
  {
    // I'm going to be surprised if this is ever not true.. it's
    // a potential problem in cc.gr, since I'm assuming there that
    // 'newHead' is not already on any other lists...
    //
    // update: This does occasionally get triggered, because a node
    // might get yielded to two contexts.  Failing this assertion is a
    // symptom that the sharing needs to be more carefully managed.
    // It's often the case that newHead->next in fact equals first()
    // already, but if the client code wants to let that slide it's
    // going to have to check itself; I don't want to silently allow
    // accidental happens-to-not-change-anything overwriting down in
    // this code.
    xassert(newHead->next == NULL);

    newHead->next = first();
    return makeList(newHead);
  }

  // random access (linear time of course)
  T const *nthC(int n) const;
  T *nth(int n) { return const_cast<T*>(nthC(n)); }

  // don't add an 'append' method; I think if you're trying to append
  // with FakeLists then you're probably misusing them

  // perhaps breaking the idea a little...
  FakeList<T> *reverse();

  // this class is deliberately sparse on methods for now, since I'm
  // not sure what kind of manipulations I'll want, given that this
  // class's purpose is fairly specialized (AST lists)
};



// I'm deliberately contradicting the convention elsewhere, where
// "FOREACH" comes first; I think it should have come second to begin
// with, and since this class isn't derived from any of the others
// with the opposite convention, this is as good a place as any to
// reverse it

#define FAKELIST_FOREACH(NodeType, listPtr, nodePtrVar)   \
  for (NodeType const *nodePtrVar = listPtr->firstC();    \
       nodePtrVar != NULL;                                \
       nodePtrVar = nodePtrVar->next)

#define FAKELIST_FOREACH_NC(NodeType, listPtr, nodePtrVar)   \
  for (NodeType *nodePtrVar = listPtr->first();              \
       nodePtrVar != NULL;                                   \
       nodePtrVar = nodePtrVar->next)


template <class T>
void FakeList<T>::deallocNodes()
{
  T *p = first();
  while (p) {
    T *next = p->next;

    // just in case T's destructor thinks it owns 'next',
    // nullify it since I'm going to dealloc it myself
    p->next = NULL;
    delete p;

    p = next;
  }
}


template <class T>
int FakeList<T>::count() const
{
  int ct = 0;
  FAKELIST_FOREACH(T, this, p) {
    ct++;
  }
  return ct;
}


template <class T>
T const *FakeList<T>::nthC(int n) const
{
  const FakeList<T> *p = this;
  while (n > 0) {
    p = p->butFirstC();  // segfaults if n is too small
    n--;
  }
  return p->firstC();
}


template <class T>
FakeList<T> *FakeList<T>::reverse()
{
  FakeList<T> *src = this;
  FakeList<T> *dest = emptyList();

  while (src->isNotEmpty()) {
    // remove first element of 'src'
    T *first = src->first();
    src = src->butFirst();
    first->next = NULL;

    // put it at the head of 'dest'
    dest = dest->prepend(first);
  }

  return dest;
}


#endif // FAKELIST_H
