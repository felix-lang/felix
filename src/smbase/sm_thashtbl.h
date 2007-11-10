// thashtbl.h            see license.txt for copyright and terms of use
// type-safe version of HashTable

#ifndef THASHTBL_H
#define THASHTBL_H

#include "sm_hashtbl.h"

template <class KEY, class DATA> class THashTableIter;

template <class KEY, class DATA>
class THashTable {
private:    // types
  friend class THashTableIter<KEY, DATA>;

public:     // types
  // given a stored data pointer, retrieve the associated key
  typedef KEY const* (*GetKeyFn)(DATA *data);

  // given a key, retrieve the associated hash value;
  // this should be a 32-bit integer ready to be mod'd by the table size
  typedef unsigned (*HashFn)(KEY const *key);

  // compare two keys; this is needed so we can handle collisions
  // in the hash function; return true if they are equal
  typedef bool (*EqualKeyFn)(KEY const *key1, KEY const *key2);

private:    // data
  // underlying table
  HashTable table;

private:    // funcs
  // disallowed
  THashTable(THashTable&);
  void operator=(THashTable&);
  void operator==(THashTable&);

public:     // funcs
  THashTable(GetKeyFn gk, HashFn hf, EqualKeyFn ek,
             int initSize = HashTable::defaultSize)
    : table((HashTable::GetKeyFn)gk,
            (HashTable::HashFn)hf,
            (HashTable::EqualKeyFn)ek,
            initSize)
  {}
  ~THashTable() {}

  // return # of mapped entries
  int getNumEntries() const                 { return table.getNumEntries(); }

  // if this hash value has a mapping, return it; otherwise,
  // return NULL
  DATA *get(KEY const *key) const           { return (DATA*)table.get(key); }

  // add a mapping from 'key' to 'value'; there must not already
  // be a mapping for this key
  void add(KEY const *key, DATA *value)     { table.add(key, value); }

  // remove the mapping for 'key' -- it must exist;
  // returns the removed item
  DATA *remove(KEY const *key)              { return (DATA*)table.remove(key); }

  // remove all mappings
  void empty(int initSize = HashTable::defaultSize)   { table.empty(initSize); }

  // set whether shrinkage is allowed; it's useful to be able to
  // disable this to avoid any allocation in certain situations
  void setEnableShrink(bool en)             { table.setEnableShrink(en); }

  // allow external access to an accessor function
  KEY const *callGetKeyFn(DATA *data)       { return (KEY const*)table.getKey(data); }

  // check the data structure's invariants, and throw an exception
  // if there is a problem
  void selfCheck() const                    { table.selfCheck(); }
};


// iterate over all stored values in a THashTable
// NOTE: you can't change the table while an iter exists
template <class KEY, class DATA>
class THashTableIter {
private:      // data
  HashTableIter iter;            // underlying iter

public:       // funcs
  THashTableIter(THashTable<KEY,DATA> &table) : iter(table.table) {}

  bool isDone() const          { return iter.isDone(); }
  void adv()                   { return iter.adv(); }
  DATA *data() const           { return (DATA*)iter.data(); }
};


#endif // THASHTBL_H
