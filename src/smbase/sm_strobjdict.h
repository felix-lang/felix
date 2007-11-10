// strobjdict.h            see license.txt for copyright and terms of use
// dictionary of objects, indexed by sm_string (case-sensitive)
// (c) Scott McPeak, 2000

#ifndef __STROBJDICT_H
#define __STROBJDICT_H

#include "sm_svdict.h"


// the dictionary object is considered to own all of the things
// contained, so constness means constness of the contained objects
// as well as the mapping from sm_strings to them

template <class T>
class StringObjDict {
public:     // types
  // 'foreach' iterator functions
  typedef bool (*ForeachCFn)(sm_string const &key, T const *value, void *extra);
  typedef bool (*ForeachFn)(sm_string const &key, T * /*serf*/ value, void *extra);

  // external iterator
  class Iter {
  private:
    StringVoidDict::IterC iter;

  public:
    Iter(StringObjDict const &dict) : iter(dict.dict) {}
    Iter(Iter const &obj) : DMEMB(iter) {}
    Iter& operator= (Iter const &obj) { CMEMB(iter); return *this; }

    bool isDone() const { return iter.isDone(); }
    Iter& next() { iter.next(); return *this; }

    sm_string const &key() const { return iter.key(); }
    T const *&value() const { return (T const *&)iter.value(); }
  };
  friend class Iter;

private:    // data
  // underlying dictionary functionality
  StringVoidDict dict;

private:    // funcs
  // disallowed
  StringObjDict(StringObjDict const &obj);
  StringObjDict& operator= (StringObjDict const &obj);
  bool operator== (StringObjDict const &obj) const;

public:     // funcs
  StringObjDict() : dict() {}
  ~StringObjDict() { empty(); }

  // due to similarity with StringVoidDict, see svdict.h for
  // details on these functions' interfaces

  // ------- selectors ---------
  int size() const                                     { return dict.size(); }

  bool isEmpty() const                                 { return dict.isEmpty(); }
  bool isNotEmpty() const                              { return !isEmpty(); }

  bool queryC(char const *key, T const *&value) const  { return dict.query(key, (void*&)value); }
  bool query(char const *key, T *&value)               { return queryC(key, (T const*&)value); }

  T const *queryfC(char const *key) const              { return (T const *)dict.queryf(key); }
  T * /*serf*/ queryf(char const *key)                 { return (T*)dict.queryf(key); }
  T * /*serf*/ queryif(char const *key)                { return (T*)dict.queryif(key); }

  bool isMapped(char const *key) const                 { return dict.isMapped(key); }

  // -------- mutators -----------
  void add(char const *key, T *value)                  { dict.add(key, value); }

  T * /*owner*/ remove(char const *key)                { return (T*)dict.remove(key); }
  void deleteAt(char const *key)                       { deleteObject(remove(key)); }

  void empty()               { dict.emptyAndDel((StringVoidDict::DelFn)deleteObject); }

  // --------- iters -------------
  void foreachC(ForeachCFn func, void *extra=NULL) const
    { dict.foreach((StringVoidDict::ForeachFn)func, extra); }
  void foreach(ForeachFn func, void *extra=NULL)
    { dict.foreach((StringVoidDict::ForeachFn)func, extra); }

  // ------------ misc --------------
  static void deleteObject(T *obj);
};


template <class T>
STATICDEF void StringObjDict<T>::deleteObject(T *obj)
{
  delete obj;
}

#endif // __STROBJDICT_H
