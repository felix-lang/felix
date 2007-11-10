// oobjmap.h            see license.txt for copyright and terms of use
// owner object map, implemented with a hash table
// maps pointer-to-key-object to pointer-to-value-object, and owns
// all instances of value-object (they are deallocated when the
// map itself goes away)

// I had envisioned an interface which didn't require the function
// that maps values back to keys.. given that OwnerHashTable requires
// this, I'll suspend this line of work for now and keep using
// OwnerHashTable directly
#error This does not work quite right

#ifndef OOBJMAP_H
#define OOBJMAP_H

#include "sm_ohashtbl.h"

template <class Key, class Value> class OObjMapIter;

template <class Key, class Value>
class OObjMap {
public:     // types
  friend class OObjMapIter<Key, Value>;

private:    // data
  OwnerHashTable<Value> table;            // implementation

public:     // funcs
  OObjMap() {}
  ~OObjMap() {}                           // deallocates Value objects

  int getNumEntries() const               { return table.getNumEntries(); }
  Value *get(Key const *key) const        { return table.get(key); }
  void add(Key const *key, Value *value)  { table.add(key, value); }
  Value *remove(Key const *key)           { return table.remove(key); }
  void empty()                            { table.empty(); }
  void selfCheck() const                  { table.selfCheck(); }
};


template <class Key, class Value>
class OObjMapIter {
private:    // data
  OwnerHashTableIter<Value> iter;         // implementation

public:
  OObjMapIter(OObjMap<Key,Value> &map)    : iter(map.table) {}

  bool isDone() const                     { return iter.isDone(); }
  void adv()                              { iter.adv(); }
  Value *data()                           { return iter.data(); }
};


#endif // OOBJMAP_H
