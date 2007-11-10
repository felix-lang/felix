// strdict.h            see license.txt for copyright and terms of use
// sm_string dictionary
// (c) Scott McPeak, 2000

// entire module is case sensitive

#ifndef __STRDICT_H
#define __STRDICT_H

#include <iostream>   // std::ostream
#include "sm_str.h"
#include "sm_macros.h"
#include "sm_xassert.h"
#include "sm_typ.h"

class StringDict {
private:    // types
  class Node {
  public:
    Node *next;
    sm_string key, value;

  public:
    Node(char const *k, char const *v, Node *n = NULL)
      : next(n), key(k), value(v) {}
    ~Node() {}
  };

public:     // types
  // Note: some care must be taken when dealing with Iters, because
  //       they can be invalidated when held across modifications to
  //       structure of the underlying dictionary
  class Iter {
  private:
    Node *current;

  public:
    Iter(Node *n) : current(n) {}
    Iter(StringDict &dict) { operator=(dict.getIter()); }
    Iter(Iter const &obj) : DMEMB(current) {}
    Iter& operator= (Iter const &obj) { CMEMB(current); return *this; }

    bool isDone() const { return current == NULL; }
    Iter& next() { xassert(current); current = current->next; return *this; }
      // 'next' returns a value primarily to allow use in for-loop comma exprs

    sm_string& key() const { return current->key; }
    sm_string& value() const { return current->value; }
  };
  friend class Iter;

  // iterator that can't modify the dictionary entries
  class IterC : protected Iter {
  public:
    IterC(Node const *n) : Iter(const_cast<Node*>(n)) {}
    IterC(StringDict const &dict) : Iter(const_cast<StringDict&>(dict)) {}
    IterC(IterC const &obj) : Iter(obj) {}
    IterC& operator= (IterC const &obj) { Iter::operator=(obj); return *this; }

    // some operations can be made available unchanged
    Iter::isDone;
    Iter::next;

    // others must be const-ified
    sm_string const &key() const { return Iter::key(); }
    sm_string const &value() const { return Iter::value(); }
  };

private:    // data
  Node *top;             // first list node (possibly NULL)

protected:  // funcs
  void selfCheck() const;      // throw exception if invariants violated

  void verifySorted() const;   // throw exception if list isn't sorted

  void /*mutable*/ sort();     // arrange nodes in alphabetically sorted order
    // (mutable because this isn't supposed to be visible from the outside)

  // invariants:
  //   list is well-formed structurally

public:
  StringDict();          // initializes to empty dictionary
  StringDict(StringDict const &obj);
  ~StringDict();

  StringDict& operator= (StringDict const &obj);

  bool operator== (StringDict const &obj) const;
  NOTEQUAL_OPERATOR(StringDict)

  // ------- selectors ---------
  int size() const;
    // retrieve # of mappings

  bool isEmpty() const;
    // returns true if size() is 0

  bool isNotEmpty() const
    { return !isEmpty(); }

  bool query(char const *key, sm_string &value) const;
    // if 'key' is mapped to a value, put it into 'value' and return true;
    // otherwise, return false

  sm_string queryf(char const *key) const;
    // return the value corresponding to 'key', or throw an exception of it's
    // not mapped

  bool isMapped(char const *key) const;
    // return true if 'key' is mapped to a value

  // -------- mutators -----------
  void add(char const *key, char const *value);
    // add a mapping from 'key' to 'value'; 'key' must initially be unmapped

  void modify(char const *key, char const *newValue);
    // change the existing value for 'key', which must exist, to 'newValue'

  void remove(char const *key);
    // remove the mapping from 'key', which must exist

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

  // ------------ misc --------------
  INSERT_OSTREAM(StringDict)
  sm_string toString() const;
};

#endif // __STRDICT_H
