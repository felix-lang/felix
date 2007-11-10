// strsobjdict.h            see license.txt for copyright and terms of use
// dictionary of *serf* pointers to objects, indexed by sm_string (case-sensitive)
// (c) Scott McPeak, 2000

// created by copying strobjdict and modifying.. nonideal..

#ifndef __STRSOBJDICT_H
#define __STRSOBJDICT_H

#include "sm_svdict.h"


// since the dictionary does not own the pointed-to objects,
// it has the same constness model as StringVoidDict, namely
// that const means the *mapping* is constant but not the
// pointed-to objects

template <class T>
class StringSObjDict {
public:     // types
  // 'foreach' iterator functions
  typedef bool (*ForeachFn)(sm_string const &key, T *value, void *extra);

  // external iterator
  class Iter {
  private:
    StringVoidDict::Iter iter;

  public:
    Iter(StringSObjDict &dict) : iter(dict.dict) {}
    Iter(Iter const &obj) : DMEMB(iter) {}
    Iter& operator= (Iter const &obj) { CMEMB(iter); return *this; }

    bool isDone() const { return iter.isDone(); }
    Iter& next() { iter.next(); return *this; }

    sm_string const &key() const { return iter.key(); }
    T *&value() const { return (T *&)iter.value(); }

    //int private_getCurrent() const { return iter.private_getCurrent(); }
    void *private_getCurrent() const { return iter.private_getCurrent(); }
  };
  friend class Iter;

  class IterC {
  private:
    StringVoidDict::IterC iter;

  public:
    IterC(StringSObjDict const &dict) : iter(dict.dict) {}
    IterC(IterC const &obj) : DMEMB(iter) {}
    IterC& operator= (IterC const &obj) { CMEMB(iter); return *this; }

    bool isDone() const { return iter.isDone(); }
    IterC& next() { iter.next(); return *this; }

    sm_string const &key() const { return iter.key(); }
    T *value() const { return (T *)iter.value(); }

    //int private_getCurrent() const { return iter.private_getCurrent(); }
    void *private_getCurrent() const { return iter.private_getCurrent(); }
  };
  friend class IterC;

private:    // data
  // underlying dictionary functionality
  StringVoidDict dict;

public:     // funcs
  StringSObjDict() : dict() {}
  StringSObjDict(StringSObjDict const &obj) : dict(obj.dict) {}
  ~StringSObjDict() {}

  // comparison and assignment use *pointer* comparison/assignment

  StringSObjDict& operator= (StringSObjDict const &obj)    { dict = obj.dict; return *this; }

  bool operator== (StringSObjDict const &obj) const        { return dict == obj.dict; }
  NOTEQUAL_OPERATOR(StringSObjDict)

  // due to similarity with StringVoidDict, see svdict.h for
  // details on these functions' interfaces

  // ------- selectors ---------
  int size() const                                     { return dict.size(); }

  bool isEmpty() const                                 { return dict.isEmpty(); }
  bool isNotEmpty() const                              { return !isEmpty(); }

  bool query(char const *key, T *&value) const         { return dict.query(key, (void*&)value); }
  T *queryf(char const *key) const                     { return (T*)dict.queryf(key); }
  T *queryif(char const *key) const                    { return (T*)dict.queryif(key); }

  bool isMapped(char const *key) const                 { return dict.isMapped(key); }

  // -------- mutators -----------
  void add(char const *key, T *value)                  { dict.add(key, (void*)value); }

  T *modify(char const *key, T *newValue)              { return (T*)dict.modify(key, (void*)newValue); }

  T *remove(char const *key)                           { return (T*)dict.remove(key); }

  void empty()                                         { dict.empty(); }

  // --------- iters -------------
  void foreach(ForeachFn func, void *extra=NULL) const
    { dict.foreach((StringVoidDict::ForeachFn)func, extra); }

  // debugging
  //int private_getTopAddr() const { return dict.private_getTopAddr(); }
  void *private_getTopAddr() const { return dict.private_getTopAddr(); }
};


#endif // __STRSOBJDICT_H
