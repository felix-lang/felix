// svdict.h            see license.txt for copyright and terms of use
// dictionary of void*, indexed by sm_string (case-sensitive)
// (c) Scott McPeak, 2000

// created by modifying strdict; at some point strdict should
// be rewritten to use this module

#ifndef __SVDICT_H
#define __SVDICT_H

#include <iostream>   // std::ostream
#include "sm_str.h"
#include "sm_macros.h"
#include "sm_xassert.h"
#include "sm_typ.h"
#include "sm_strhash.h"


// constness: for this class, 'const' means the *mapping* from sm_string
// to void* won't change; but I don't prevent the thing pointed-at
// by the void* from changing (would like multiple levels of constness..)

class StringVoidDict {
private:    // types
  // 3/16/03: I believe the reason I stored the information both in a
  // hash table and in a linked list is to be able to support efficient
  // alphabetical iteration
  class Node {
  public:
    Node *next;
    sm_string key;
    void *value;

  public:
    Node(char const *k, void *v, Node *n = NULL)
      : next(n), key(k), value(v) {}
    ~Node() {}

    static char const *getKey(Node const *n);
  };

public:     // types
  // function for general foreach; return false to continue,
  // true to stop iterating
  typedef bool (*ForeachFn)(sm_string const &key, void *value, void *extra);

  // function type to delete void*s while emptying
  typedef void (*DelFn)(void *value);

  // Note: some care must be taken when dealing with Iters, because
  //       they can be invalidated when held across modifications to
  //       structure of the underlying dictionary
  class Iter {
  private:
    Node *current;

  public:
    Iter(Node *n) : current(n) {}
    Iter(StringVoidDict &dict) { operator=(dict.getIter()); }
    Iter(Iter const &obj) : DMEMB(current) {}
    Iter& operator= (Iter const &obj) { CMEMB(current); return *this; }

    bool isDone() const { return current == NULL; }
    Iter& next() { xassert(current); current = current->next; return *this; }
      // 'next' returns a value primarily to allow use in for-loop comma exprs

    sm_string &key() const { return current->key; }
    void *&value() const { return current->value; }

    //int private_getCurrent() const { return (int)current; }
    void *private_getCurrent() const { return current; }
  };
  friend class Iter;

  // iterator that can't modify the dictionary entries
  class IterC : protected Iter {
  public:
    IterC(Node const *n) : Iter(const_cast<Node*>(n)) {}
    IterC(StringVoidDict const &dict) : Iter(const_cast<StringVoidDict&>(dict)) {}
    IterC(IterC const &obj) : Iter(obj) {}
    IterC& operator= (IterC const &obj) { Iter::operator=(obj); return *this; }

    // some operations can be made available unchanged
    Iter::isDone;
    Iter::next;
    Iter::private_getCurrent;

    // others must be const-ified
    sm_string const &key() const { return Iter::key(); }
    void const *&value() const { return (void const *&)Iter::value(); }
  };

private:    // data
  // first list node (possibly NULL)
  Node *top;

  // hash table to improve lookup performance
  StringHash hash;

  // invariants:
  //   list is well-formed structurally
  //   every node is in the hash table, and vice versa

protected:  // funcs
  void selfCheck() const;      // throw exception if invariants violated

  void verifySorted() const;   // throw exception if list isn't sorted

  void /*mutable*/ sort();     // arrange nodes in alphabetically sorted order
    // (mutable because this isn't supposed to be visible from the outside)

public:
  StringVoidDict();          // initializes to empty dictionary
  StringVoidDict(StringVoidDict const &obj);
  ~StringVoidDict();

  StringVoidDict& operator= (StringVoidDict const &obj);

  // comparison is done by pointer equality of void*
  bool operator== (StringVoidDict const &obj) const;
  NOTEQUAL_OPERATOR(StringVoidDict)

  // ------- selectors ---------
  int size() const;
    // retrieve # of mappings

  bool isEmpty() const;
    // returns true if size() is 0

  bool isNotEmpty() const
    { return !isEmpty(); }

  bool query(char const *key, void *&value) const;
    // if 'key' is mapped to a value, put it into 'value' and return true;
    // otherwise, return false

  void *queryf(char const *key) const;
    // return the value corresponding to 'key', or throw an exception of it's
    // not mapped

  void *queryif(char const *key) const;
    // return the value corresponding to 'key', or return NULL

  bool isMapped(char const *key) const;
    // return true if 'key' is mapped to a value

  // -------- mutators -----------
  void add(char const *key, void *value);
    // add a mapping from 'key' to 'value'; 'key' must initially be unmapped

  void *modify(char const *key, void *newValue);
    // change the existing value for 'key', which must exist, to 'newValue';
    // the old value is returned

  void *remove(char const *key);
    // remove the mapping from 'key', which must exist; it is returned

  void emptyAndDel(DelFn func);
    // apply the deletion func to all values, during empty()

  void empty();
    // remove all mappings

  // --------- iters -------------
  Iter getIter();
    // retrieve an iterator (the iterator remains valid only as long as
    // the structure of the dictionary does not get modified);
    // values will be iterated in *alphabetical* order

  IterC getIterC() const;
    // retrieve a const iterator

  Iter find(char const *key);
    // return an iterator pointing to 'key', or an iterator
    // that isDone() if 'key' isn't mapped

  void foreach(ForeachFn func, void *extra=NULL) const;
    // apply 'func' to every mapping, in alphabetical order

  // ------------ misc --------------
  INSERT_OSTREAM(StringVoidDict)
  sm_string toString() const;

  // debugging...
  //int private_getTopAddr() const { return (int)top; }
  void *private_getTopAddr() const { return top; }
};

#endif // __SVDICT_H
