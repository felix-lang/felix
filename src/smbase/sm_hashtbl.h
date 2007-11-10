// hashtbl.h            see license.txt for copyright and terms of use
// hash table mapping arbitrary keys to void*, where
// the stored pointers can be used to derive the key,
// and cannot be NULL

#ifndef HASHTBL_H
#define HASHTBL_H

#include "sm_typ.h"

class HashTable {
private:    // types
  friend class HashTableIter;

public:     // types
  // given a stored data pointer, retrieve the associated key
  typedef void const* (*GetKeyFn)(void *data);

  // given a key, retrieve the associated hash value;
  // this should be a 32-bit integer ready to be mod'd by the table size
  typedef unsigned (*HashFn)(void const *key);

  // compare two keys; this is needed so we can handle collisions
  // in the hash function; return true if they are equal
  typedef bool (*EqualKeyFn)(void const *key1, void const *key2);

  // constants
  enum {
    defaultSize = 33
  };

private:    // data
  // maps
  GetKeyFn getKey;
  HashFn coreHashFn;
  EqualKeyFn equalKeys;

  // array of pointers to data, indexed by the hash value,
  // with collisions resolved by moving to adjacent entries;
  // some entries are NULL, meaning that hash value has no mapping
  void **hashTable;

  // Why use linear hashing instead of double hashing?  To support
  // deletion.  Since every probe sequence that goes through index k
  // will have a tail of k+1,k+2,... (mod tableSize) I can easily find
  // and re-insert all the elements whose position might have depended
  // on the presence of a now-deleted element.  Excessive clustering
  // is (hopefully) avoided through load factor control.

  // number of slots in the hash table
  int tableSize;

  // number of mapped (non-NULL) entries
  int numEntries;

  // when false, we never make the table smaller (default: true)
  bool enableShrink;

private:    // funcs
  // disallowed
  HashTable(HashTable&);
  void operator=(HashTable&);
  void operator==(HashTable&);

  // hash fn for the current table size; always in [0,tableSize-1]
  unsigned hashFunction(void const *key) const;

  // given a collision at 'index', return the next index to try
  int nextIndex(int index) const { return (index+1) % tableSize; }

  // resize the table, transferring all the entries to their
  // new positions
  void resizeTable(int newSize);

  // return the index of the entry corresponding to 'data' if it
  // is mapped, or a pointer to the entry that should be filled
  // with its mapping, if unmapped
  int getEntry(void const *key) const;

  // make a new table with the given size
  void makeTable(int size);

  // check a single entry for integrity
  void checkEntry(int entry) const;

public:     // funcs
  HashTable(GetKeyFn gk, HashFn hf, EqualKeyFn ek,
            int initSize = HashTable::defaultSize);
  ~HashTable();

  // return # of mapped entries
  int getNumEntries() const { return numEntries; }

  // if this hash value has a mapping, return it; otherwise,
  // return NULL
  void *get(void const *key) const;

  // add a mapping from 'key' to 'value'; there must not already
  // be a mapping for this key
  void add(void const *key, void *value);

  // remove the mapping for 'key' -- it must exist
  // returns the removed item
  void *remove(void const *key);

  // remove all mappings
  void empty(int initSize = HashTable::defaultSize);

  // set whether shrinkage is allowed; it's useful to be able to
  // disable this to avoid any allocation in certain situations
  void setEnableShrink(bool en) { enableShrink = en; }

  // allow external access to an accessor function
  void const *callGetKeyFn(void *data) { return getKey(data); }

  // check the data structure's invariants, and throw an exception
  // if there is a problem
  void selfCheck() const;

  // ------ useful default functions -------
  // returns its argument
  static void const* identityKeyFn(void *data);

  // puts the argument through two cycles of a linear
  // congruential pseudo-random number generator
  static unsigned lcprngHashFn(void const *key);

  // does pointer equality comparison
  static bool pointerEqualKeyFn(void const *key1, void const *key2);
};

unsigned lcprngTwoSteps(unsigned v);


// iterate over all stored values in a HashTable
// NOTE: you can't change the table while an iter exists
class HashTableIter {
private:      // data
  HashTable &table;      // table we're iterating over
  int index;             // current slot to return in adv(); -1 when done

private:      // funcs
  void moveToSth();

public:       // funcs
  HashTableIter(HashTable &table);

  bool isDone() const { return index == -1; }
  void adv();
  void *data() const;          // returns a value stored in the table
};


#endif // HASHTBL_H
