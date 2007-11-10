// strhash.h            see license.txt for copyright and terms of use
// hash table mapping sm_strings to arbitrary pointers, where
// the stored pointers can be used to derive the key, and
// cannot be NULL

#ifndef STRHASH_H
#define STRHASH_H

#include "sm_hashtbl.h"

class StringHash : private HashTable {
public:     // types
  // given a stored data pointer, retrieve the associated key
  typedef char const* (*GetKeyFn)(void *data);

private:    // funcs
  // disallowed
  StringHash(StringHash&);
  void operator=(StringHash&);
  void operator==(StringHash&);

public:     // funcs
  StringHash(GetKeyFn getKey);
  ~StringHash();

  // utilities
  static unsigned coreHash(char const *key);
  static bool keyCompare(char const *key1, char const *key2);

  // return # of mapped entries
  int getNumEntries() const
    { return HashTable::getNumEntries(); }

  // if this key has a mapping, return it; otherwise,
  // return NULL
  void *get(char const *key) const
    { return HashTable::get(key); }

  // add a mapping from 'key' to 'value'; there must not already
  // be a mapping for this key
  void add(char const *key, void *value)
    { HashTable::add(key, value); }

  // remove the mapping for 'key' -- it must exist
  void remove(char const *key)
    { HashTable::remove(key); }

  // drop all entries
  void empty()
    { HashTable::empty(); }

  // check the data structure's invariants, and throw an exception
  // if there is a problem
  void selfCheck() const
    { HashTable::selfCheck(); }
};


// type-safe template wrapper
template <class T>
class TStringHash : public StringHash {
public:      // types
  typedef char const* (*GetKeyFn)(T *data);

public:
  TStringHash(GetKeyFn fn)            : StringHash((StringHash::GetKeyFn)fn) {}
  ~TStringHash()                      {}

  int getNumEntries() const           { return StringHash::getNumEntries(); }
  T *get(char const *key) const       { return (T*)StringHash::get(key); }
  void add(char const *key, T *value) { StringHash::add(key, (void*)value); }
  void remove(char const *key)        { StringHash::remove(key); }
  void empty()                        { StringHash::empty(); }
};


#endif // STRHASH_H
