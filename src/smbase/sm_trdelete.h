// trdelete.h            see license.txt for copyright and terms of use
// objects which trash their contents upon deletion
// I would love to have implemented this as a base class and simply derive
//   things from it, but a poor implementation choice by Borland makes this
//   too costly in terms of performance

#ifdef _MSC_VER
  // this module doesn't work under msvc, I don't care to figure out why
  #define TRDELETE_H      // make it omit this file
  #define TRASHINGDELETE  // and all references to it a no-op
#endif

#ifndef TRDELETE_H
#define TRDELETE_H

#include <stddef.h>      // size_t

void trashingDelete(void *blk, size_t size);
void trashingDeleteArr(void *blk, size_t size);

// to use, include the TRASHINGDELETE macro in the public section of a class

#define TRASHINGDELETE                                                              \
  void operator delete(void *blk, size_t size) { trashingDelete(blk, size); }       \
  void operator delete[](void *blk, size_t size) { trashingDeleteArr(blk, size); }

#endif // TRDELETE_H
