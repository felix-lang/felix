// svdict.cc            see license.txt for copyright and terms of use
// code for svdict.h

#include "sm_svdict.h"
#include <cstring>         // strcmp


#define FOREACH_NODE(itervar) \
  for(Node *itervar = top; itervar != NULL; itervar = itervar->next)

#define FOREACH_ITER(dict, itervar) \
  for(Iter itervar = (dict).getIter(); !itervar.isDone(); itervar.next())

#define FOREACH_ITERC(dict, itervar) \
  for(IterC itervar = (dict).getIterC(); !itervar.isDone(); itervar.next())

#define MUTABLE_SORT(obj) (const_cast<StringVoidDict&>(obj)).sort()


STATICDEF char const *StringVoidDict::Node::getKey(StringVoidDict::Node const *n)
{
  return n->key.pcharc();
}


StringVoidDict::StringVoidDict()
  : top(NULL),
    hash((StringHash::GetKeyFn)Node::getKey)
{}


StringVoidDict::StringVoidDict(StringVoidDict const &obj)
  : top(NULL),
    hash((StringHash::GetKeyFn)Node::getKey)
{
  *this = obj;
}


StringVoidDict::~StringVoidDict()
{
  SELFCHECK();
  empty();
}


StringVoidDict& StringVoidDict::operator= (StringVoidDict const &obj)
{
  if (this == &obj) {
    return *this;
  }

  empty();

  Node *end = top;
  FOREACH_ITERC(obj, src) {
    // const_cast needed because the resulting dictionary can now access
    // the data objects and modify them... hmm...
    Node *newnode = new Node(src.key(), const_cast<void*>(src.value()));
    if (!end) {
      // first element of list
      end = top = newnode;
    }
    else {
      // adding to end of nonempty list
      end = end->next = newnode;
    }
    hash.add(newnode->key, newnode);
  }

  SELFCHECK();
  return *this;
}


bool StringVoidDict::operator== (StringVoidDict const &obj) const
{
  // sort both lists
  MUTABLE_SORT(*this);
  MUTABLE_SORT(obj);

  IterC ths(*this), other(obj);
  while (!ths.isDone() && !other.isDone()) {
    if (0!=std::strcmp(ths.key(), other.key()) ||
        ths.value() != other.value()) {
      return false;
    }
    ths.next();
    other.next();
  }

  if (!ths.isDone() || !other.isDone()) {
    // one finished first, so they can't be equal
    return false;
  }

  return true;
}


bool StringVoidDict::isEmpty() const
{
  return top == NULL;
}


int StringVoidDict::size() const
{
  return hash.getNumEntries();
}


bool StringVoidDict::query(char const *key, void *&value) const
{
  Node *n = (Node*)hash.get(key);
  if (!n) {
    return false;
  }

  value = n->value;
  return true;
}


void *StringVoidDict::queryf(char const *key) const
{
  void *ret;
  bool ok = query(key, ret);
  xassert(ok);
  return ret;
}


void *StringVoidDict::queryif(char const *key) const
{
  void *ret;
  if (query(key, ret)) {
    return ret;
  }
  else {
    return NULL;
  }
}


bool StringVoidDict::isMapped(char const *key) const
{
  void *dummy;
  return query(key, dummy);
}


void StringVoidDict::add(char const *key, void *value)
{
  xassert(!isMapped(key));

  // just prepend; we'll sort later (when an iterator is retrieved)
  top = new Node(key, value, top);
  hash.add(key, top);

  SELFCHECK();
}


void *StringVoidDict::modify(char const *key, void *newValue)
{
  Iter entry = find(key);
  xassert(!entry.isDone());

  void *ret = entry.value();
  entry.value() = newValue;

  SELFCHECK();
  return ret;
}


StringVoidDict::Iter StringVoidDict::find(char const *key)
{
  Node *n = (Node*)hash.get(key);
  return Iter(n);
}


void *StringVoidDict::remove(char const *key)
{
  void *ret;     // will be previous value
  xassert(top);

  // check for removal of top element
  if (0==std::strcmp(top->key, key)) {
    Node *temp = top;
    top = top->next;
    ret = temp->value;
    hash.remove(temp->key);
    delete temp;
  }

  // find node to remove in tail of list
  else {
    Node *p = top;
    while (p->next && 0!=std::strcmp(p->next->key, key)) {
      p = p->next;
    }

    if (!p->next) {
      // reached the end of the list without finding the key
      xfailure("failed to find key");
    }

    // remove p->next from the list
    Node *temp = p->next;
    p->next = p->next->next;
    ret = temp->value;
    hash.remove(temp->key);
    delete temp;
  }

  SELFCHECK();
  return ret;
}


void StringVoidDict::empty()
{
  emptyAndDel(NULL);
}

void StringVoidDict::emptyAndDel(DelFn func)
{
  while (top) {
    Node *temp = top;
    top = top->next;

    if (func != NULL) {
      func(temp->value);
    }
    hash.remove(temp->key);
    delete temp;
  }

  SELFCHECK();
}


StringVoidDict::Iter StringVoidDict::getIter()
{
  sort();        // must return items in sorted order
  return Iter(top);
}


StringVoidDict::IterC StringVoidDict::getIterC() const
{
  //sort();
  const_cast<StringVoidDict*>(this)->sort();    // mutable
  return IterC(top);
}


void StringVoidDict::foreach(ForeachFn func, void *extra) const
{
  const_cast<StringVoidDict*>(this)->sort();    // mutable

  for (Node *n = top; n != NULL; n = n->next) {
    if (func(n->key, n->value, extra)) {
      return;
    }
  }
}


// use simple insertion sort for now
/*mutable*/ void StringVoidDict::sort()
{
  if (!top) {
    return;
  }

  // invariant: sequence of nodes from 'top' to 'walker', inclusive,
  //            is always sorted
  Node *walker = top;
  while (walker->next != NULL) {
    // see if walker->next is out of order
    if (0 <= std::strcmp(walker->key, walker->next->key)) {
      // it's in order
      walker = walker->next;
      continue;
    }

    // remove walker->next from where it is (note that this has
    // the effect of advancing walker, so below here we won't
    // have another movement of walker)
    Node *mover = walker->next;
    walker->next = walker->next->next;
    mover->next = NULL;       // (redundant because of (**) lines)

    // insert at head?
    if (0 < std::strcmp(mover->key, top->key)) {
      mover->next = top;            // (**)
      top = mover;
      continue;
    }

    // must find correct place to insert mover (will find the place
    // where we can insert mover just before searcher->next)
    Node *searcher = top;
    while (0 < std::strcmp(searcher->next->key, mover->key)) {
      searcher = searcher->next;
      xassert(searcher != walker);
        // otherwise how could mover have been out of order to begin with?
    }

    // insert mover before searcher->next
    mover->next = searcher->next;   // (**)
    searcher->next = mover;
  }

  SELFCHECK();

  #ifndef NDEBUG
    verifySorted();
  #endif
}


void StringVoidDict::verifySorted() const
{
  if (!top) {
    return;
  }

  Node *p = top;
  while (p->next) {
    xassert(0 <= std::strcmp(p->key, p->next->key));
    p = p->next;
  }
}


// verify that the list is well structured
void StringVoidDict::selfCheck() const
{
  {
    Node *fast = top, *slow = top;
    while (fast && fast->next) {
      fast = fast->next->next;
      slow = slow->next;

      xassert(fast != slow);
        // if these become equal, the list is circular
    }
  }

  // check counts, mappings
  int ct=0;
  for (Node *n = top; n != NULL; n = n->next, ct++) {
    xassert(hash.get(n->key) == n);
  }
  xassert(hash.getNumEntries() == ct);
}


void StringVoidDict::insertOstream(std::ostream &os) const
{
  FOREACH_ITERC(*this, entry) {
    os << entry.key() << " = " << entry.value() << std::endl;
  }
}


sm_string StringVoidDict::toString() const
{
  sm_stringBuilder sb;
  sb << "{";
  int count=0;
  FOREACH_ITERC(*this, entry) {
    if (count++ > 0) {
      sb << ",";
    }
    sb << " " << entry.key() << "=\"" << entry.value() << "\"";
  }
  sb << " }";
  return sb;
}


// -------------------- test code ------------------------
#ifdef TEST_SVDICT

#include "sm_test.h"
#include <stdlib.h>    // rand

#define myrandom(n) (rand()%(n))

char randChar()
{
  return (char)(myrandom(127-32+1)+32);
}

sm_string randString(int len)
{
  sm_stringBuilder str;
  loopj(len) {
    str << randChar();
  }
  return str;
}

sm_string randStringRandLen(int maxlen)
{
  return randString(myrandom(maxlen)+1);
}

sm_string randKey(StringVoidDict const &dict)
{
  int size = dict.size();
  xassert(size > 0);

  int nth = myrandom(size);
  StringVoidDict::IterC entry(dict);
  for (; nth > 0; entry.next(), nth--)
    {}

  return entry.key();
}

void *randVoidPtr()
{
  return (void*)(myrandom(100) * 8);
}


void entry()
{
  StringVoidDict dict;
  int size=0, collisions=0;

  int iters = 1000;
  loopi(iters) {
    switch (myrandom(6)) {
      case 0: {
        // insert a random element
        sm_string key = randStringRandLen(10);
        void *value = randVoidPtr();

        if (!dict.isMapped(key)) {
          dict.add(key, value);
          size++;
        }
        else {
          collisions++;
        }
        break;
      }

      case 1: {
        // remove a random element
        if (dict.isEmpty()) {
          break;
        }

        sm_string key = randKey(dict);
        dict.remove(key);
        size--;
        break;
      }

      case 2: {
        // check a random element that should not be there
        sm_string key = randStringRandLen(10);
        if (dict.isMapped(key)) {
          collisions++;
        }
        break;
      }

      case 3: {
        // verify that computed length is right
        xassert(size == dict.size());
        break;
      }

      case 4: {
        // test == and =
        StringVoidDict dict2(dict);
        xassert(dict2 == dict);
        xassert(dict2.size() == dict.size());

        // modify it, then verify inequality
        if (!dict2.isEmpty()) {
          sm_string key = randKey(dict2);
          void *value = dict2.queryf(key);

          if (myrandom(2) == 0) {
            dict2.remove(key);
          }
          else {
            dict2.modify(key, (void*)((int)value + 24));
          }
          xassert(dict2 != dict);
        }

        break;
      }

      case 5: {
        // random modification
        if (!dict.isEmpty()) {
          sm_string key = randKey(dict);
          dict.modify(key, randVoidPtr());
        }
        break;
      }

      default:
        xfailure("huh?");
        break;
    }
  }

  std::cout << "final size: " << size
       << "\ncollisions: " << collisions
       << "\n";

  std::cout << "all tests passed\n";
}

USUAL_MAIN

#endif // TEST_STRDICT
