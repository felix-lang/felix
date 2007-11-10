// okhasharr.h            see license.txt for copyright and terms of use
// combination of an owner hash table and an array/stack
//
// in its present form, it's ideal for a worklist, but not
// for a 'finished' list, due to inability to randomly remove

#ifndef OKHASHARR_H
#define OKHASHARR_H

#include "sm_array.h"
#include "sm_okhashtbl.h"

// T is value, K is key
template <class T, class K>
class OwnerKHashArray {
private:    // data
  OwnerKHashTable<T,K> hash;
  ArrayStack<T*> stack;

public:     // funcs
  OwnerKHashArray(typename OwnerKHashTable<T,K>::GetKeyFn gk,
                  typename OwnerKHashTable<T,K>::HashFn hf,
                  typename OwnerKHashTable<T,K>::EqualKeyFn ek,
                  int initSize = HashTable::defaultSize)
    : hash(gk, hf, ek, initSize),
      stack(initSize)
  {
    hash.setEnableShrink(false);
  }
  ~OwnerKHashArray();

  // # elts in the structure
  int count() const                  { return stack.length(); }
  bool isEmpty() const               { return stack.isEmpty(); }
  bool isNotEmpty() const            { return !isEmpty(); }

  // access as a hashtable
  T *lookup(K const *key) const      { return hash.get(key); }
  K const *callGetKeyFn(T *data)     { return hash.callGetKeyFn(data); }

  // TODO: make a new base-level implementation so I can support
  // removal of arbitrary objects efficiently

  // access as a stack
  void push(K const *key, T *value) {
    hash.add(key, value);
    stack.push(value);
  }

  T *pop() {
    T *ret = stack.pop();
    hash.remove(hash.callGetKeyFn(ret));
    return ret;
  }
};


template <class T, class K>
OwnerKHashArray<T,K>::~OwnerKHashArray()
{}


#endif // OKHASHARR_H
