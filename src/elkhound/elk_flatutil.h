// flatutil.h            see license.txt for copyright and terms of use
// flatten helpers

#ifndef FLATUTIL_H
#define FLATUTIL_H

#include "sm_flatten.h"
#include "sm_objlist.h"
#include "sm_sobjlist.h"


// ------------- xfer of owners -----------------
template <class T>
void xferOwnerPtr(Flatten &flat, T *&ptr)
{
  if (flat.reading()) {
    // construct a new, empty object
    ptr = new T(flat);
  }

  // read/write it
  ptr->xfer(flat);

  // note it so we can have serfs to it
  flat.noteOwner(ptr);
}


template <class T>
void xferOwnerPtr_readObj(Flatten &flat, T *&ptr)
{
  if (flat.reading()) {
    // construct a new object, *and* read it from file
    ptr = T::readObj(flat);
  }
  else {
    // write it
    ptr->xfer(flat);
  }

  // note it so we can have serfs to it
  flat.noteOwner(ptr);
}


template <class T>
void xferObjList(Flatten &flat, ObjList <T> &list)
{
  if (flat.writing()) {
    flat.writeInt(list.count());

    MUTATE_EACH_OBJLIST(T, list, iter) {
      iter.data()->xfer(flat);
      flat.noteOwner(iter.data());
    }
  }
  else {
    int listLen = flat.readInt();

    ObjListMutator<T> mut(list);
    while (listLen--) {
      // construct a new, empty object
      T *obj = new T(flat);

      // read it
      obj->xfer(flat);
      flat.noteOwner(obj);

      // add it to the list
      mut.append(obj);
    }
  }
}


// for things like AExprNode which have a readObj
// static method .. it's possible to merge this with
// the above code, but I'm not sure that's a good idea yet
template <class T>
void xferObjList_readObj(Flatten &flat, ObjList <T> &list)
{
  if (flat.writing()) {
    flat.writeInt(list.count());

    MUTATE_EACH_OBJLIST(T, list, iter) {
      iter.data()->xfer(flat);
      flat.noteOwner(iter.data());
    }
  }
  else {
    int listLen = flat.readInt();

    ObjListMutator<T> mut(list);
    while (listLen--) {
      // construct a new object, *and* read its
      // contents from the file
      T *obj = T::readObj(flat);
      flat.noteOwner(obj);

      // add it to the list
      mut.append(obj);
    }
  }
}


// ------------- xfer of serfs -----------------
// xfer a list of serf pointers to objects, each object
// could be in one of several owner lists
template <class T>
void xferSObjList_multi(Flatten &flat, SObjList<T> &list,
                        ObjList<T> **masterLists, int numMasters)
{
  // be sure the same number of master lists are used at
  // read and write time
  flat.checkpoint(numMasters);

  if (flat.writing()) {
    flat.writeInt(list.count());

    SMUTATE_EACH_OBJLIST(T, list, iter) {
      // determine which master list it's in
      int master;
      for (master = 0; master<numMasters; master++) {
        int index = masterLists[master]->indexOf(iter.data());
        if (index != -1) {
          // we found it -- encode the list and its index
          if (numMasters > 1) {
            flat.writeInt(master);    // only do this if multiple masters
          }
          flat.writeInt(index);
          break;
        }
      }

      if (master == numMasters) {
        // failed to find the master list
        xfailure("xferSObjList_multi: obj not in any of the lists");
      }
    }
  }

  else {
    int listLen = flat.readInt();

    SObjListMutator<T> mut(list);
    while (listLen--) {
      int master = 0;               // assume just 1 master
      if (numMasters > 1) {
        master = flat.readInt();    // then refine
      }

      mut.append(masterLists[master]->nth(flat.readInt()));
    }
  }
}


// xfer a list of serf pointers to objects owner by 'masterList'
template <class T>
void xferSObjList(Flatten &flat, SObjList<T> &list, ObjList<T> &masterList)
{
  ObjList<T> *ptr = &masterList;
  xferSObjList_multi(flat, list, &ptr, 1 /*numMasters*/);
}


// xfer a pointer which points to something in a master list
template <class T>
void xferSerfPtrToList(Flatten &flat, T *&ptr, ObjList<T> &masterList)
{
  if (flat.writing()) {
    flat.writeInt(masterList.indexOfF(ptr));
  }
  else {
    ptr = masterList.nth(flat.readInt());
  }
}


template <class T>
void xferNullableSerfPtrToList(Flatten &flat, T *&ptr, ObjList<T> &masterList)
{
  if (flat.writing()) {
    flat.writeInt(masterList.indexOf(ptr));
  }
  else {
    int index = flat.readInt();
    if (index >= 0) {
      ptr = masterList.nth(index);
    }
    else {
      ptr = NULL;
    }
  }
}


template <class T>
void computedValue(Flatten &flat, T &variable, T value)
{
  if (flat.writing()) {
    // check it
    xassert(variable == value);
  }
  else {
    // set it
    variable = value;
  }
}


// void* implementation
//#define Leaf void
//#define Root void
//#define FirstLevel void
template <class Root, class FirstLevel, class Leaf>
void xferSerfPtr_twoLevelAccess(
  Flatten &flat,
  Leaf *&leaf,
  Root *root,
  FirstLevel* (*getNthFirst)(Root *r, int n),
  Leaf* (*getNthLeaf)(FirstLevel *f, int n))
{
  if (flat.writing()) {
    // determine both indices
    for (int index1=0; ; index1++) {
      // get a first-level obj
      FirstLevel *first = getNthFirst(root, index1);
      if (!first) {
        // exhausted first-level objs
        xfailure("xferSerfPtr_twoLevelAccess: couldn't find obj to xfer");
      }

      // look for the leaf inside it
      for (int index2=0; ; index2++) {
        Leaf *second = getNthLeaf(first, index2);
        if (second == leaf) {
          // found it; encode both indices
          flat.writeInt(index1);
          flat.writeInt(index2);
          return;
        }
        if (second == NULL) {
          // exhausted this subtree
          break;
        }
      } // end of iter over leaves
    } // end of iter over first-lvl objs
  }

  else /*reading*/ {
    // read both indicies
    int index1 = flat.readInt();
    int index2 = flat.readInt();

    // follow the access path
    FirstLevel *first = getNthFirst(root, index1);
    formatAssert(first != NULL);
    Leaf *second = getNthLeaf(first, index2);
    formatAssert(second != NULL);

    // found it
    leaf = second;
  }
}
//#undef Leaf
//#undef Root
//#undef FirstLevel


#if 0
typedef void *accessFunc_void(void *parent, int childNum);

// typesafe interface
template <class Root, class FirstLevel, class Leaf>
inline void xferSerfPtr_twoLevelAccess(
  Flatten &flat,
  Leaf *&leaf,
  Root *root,
  FirstLevel* (*getNthFirst)(Root *r, int n),
  Leaf* (*getNthLeaf)(FirstLevel *f, int n))
{
  xferSerfPtr_twoLevelAccess(
    flat,
    (void*&)leaf,
    (void*)root,
    (accessFunc_void)getNthFirst,
    (accessFunc_void)getNthLeaf);
}
#endif // 0


template <class Root, class FirstLevel, class Leaf>
void xferSObjList_twoLevelAccess(
  Flatten &flat,
  SObjList<Leaf> &serfList,
  Root *root,
  FirstLevel* (*getNthFirst)(Root *r, int n),
  Leaf* (*getNthLeaf)(FirstLevel *f, int n))
{
  if (flat.writing()) {
    // length of list
    flat.writeInt(serfList.count());

    // iterate over list
    SMUTATE_EACH_OBJLIST(Leaf, serfList, iter) {
      // write the obj
      Leaf *leaf = iter.data();
      xferSerfPtr_twoLevelAccess(
        flat, leaf, root,
        getNthFirst, getNthLeaf);
    }
  }
  else {
    int length = flat.readInt();

    SObjListMutator<Leaf> mut(serfList);
    while (length--) {
      // read the obj
      Leaf *leaf;
      xferSerfPtr_twoLevelAccess(
        flat, leaf, root,
        getNthFirst, getNthLeaf);

      // store it in the list
      mut.append(leaf);
    }
  }
}


template <class T>
void xferSerfPtr(Flatten &flat, T *&serfPtr)
{
  flat.xferSerf((void*&)serfPtr, false /*nullable*/);
}

template <class T>
void xferNullableSerfPtr(Flatten &flat, T *&serfPtr)
{
  flat.xferSerf((void*&)serfPtr, true /*nullable*/);
}


#endif // FLATUTIL_H
