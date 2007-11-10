// okhashtbl.h            see license.txt for copyright and terms of use
// version of ohasharr.h with type-safe keys ("k" for keys)

#ifndef OKHASHTBL_H
#define OKHASHTBL_H

#include "sm_typ.h"
#include "sm_hashtbl.h"

template <class T, class K> class OwnerKHashTableIter;

// T is the value type, K is the key type
template <class T, class K>
class OwnerKHashTable {
public:     // types
  friend class OwnerKHashTableIter<T,K>;

  // see hashtbl.h
  typedef K const* (*GetKeyFn)(T *data);
  typedef unsigned (*HashFn)(K const *key);
  typedef bool (*EqualKeyFn)(K const *key1, K const *key2);

private:    // data
  // inner table that does the hash mapping
  HashTable table;

public:     // funcs
  OwnerKHashTable(GetKeyFn gk, HashFn hf, EqualKeyFn ek,
                  int initSize = HashTable::defaultSize)
    : table((HashTable::GetKeyFn)gk,
            (HashTable::HashFn)hf,
            (HashTable::EqualKeyFn)ek,
            initSize) {}
  ~OwnerKHashTable() { empty(1); }

  int getNumEntries() const               { return table.getNumEntries(); }
  T *get(K const *key) const              { return (T*)table.get(key); }
  void add(K const *key, T *value)        { table.add(key, value); }
  T *remove(K const *key)                 { return (T*)table.remove(key); }
  void empty(int initSize = HashTable::defaultSize);
  void setEnableShrink(bool en)           { table.setEnableShrink(en); }
  K const *callGetKeyFn(T *data)          { return (K const*)table.callGetKeyFn(data); }
  void selfCheck() const                  { table.selfCheck(); }

  // this simply drops all the entries without deleting them; it is
  // useful when the objects have been taken out via iteration
  void disownAndForgetAll(int initSize = HashTable::defaultSize)
                                          { table.empty(initSize); }
};

template <class T, class K>
void OwnerKHashTable<T,K>::empty(int initSize)
{
  HashTableIter iter(table);
  for (; !iter.isDone(); iter.adv()) {
    delete (T*)iter.data();
  }
  table.empty(initSize);
}


template <class T, class K>
class OwnerKHashTableIter {
private:      // data
  HashTableIter iter;      // internal iterator

public:       // funcs
  OwnerKHashTableIter(OwnerKHashTable<T,K> &table)
    : iter(table.table) {}

  bool isDone() const      { return iter.isDone(); }
  void adv()               { iter.adv(); }
  T *data()                { return (T*)iter.data(); }
};

#endif // OKHASHTBL_H
