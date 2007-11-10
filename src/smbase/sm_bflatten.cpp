// bflatten.cc            see license.txt for copyright and terms of use
// code for bflatten.h

#include "sm_bflatten.h"
#include "sm_exc.h"
#include "sm_syserr.h"


BFlatten::BFlatten(char const *fname, bool r)
  : readMode(r),
    ownerTable(!r? &BFlatten::getOwnerPtrKeyFn : &BFlatten::getIntNameKeyFn,
               HashTable::lcprngHashFn,
               HashTable::pointerEqualKeyFn),
    nextUniqueName(1)
{
  fp = fopen(fname, readMode? "rb" : "wb");
  if (!fp) {
    throw_XOpen(fname);
  }
}

BFlatten::~BFlatten()
{
  fclose(fp);
}


STATICDEF void const* BFlatten::getOwnerPtrKeyFn(OwnerMapping *data)
{
  return data->ownerPtr;
}

STATICDEF void const* BFlatten::getIntNameKeyFn(OwnerMapping *data)
{
  return (void const*)(data->intName);
}


void BFlatten::xferSimple(void *var, unsigned len)
{
  if (writing()) {
    if (fwrite(var, 1, len, fp) < len) {
      xsyserror("fwrite");
    }
  }
  else {
    if (fread(var, 1, len, fp) < len) {
      xsyserror("fread");
    }
  }
}


void BFlatten::noteOwner(void *ownerPtr)
{
  // make a new mapping
  OwnerMapping *map = new OwnerMapping;
  map->ownerPtr = ownerPtr;
  map->intName = nextUniqueName++;

  // add it to the table
  if (writing()) {
    // index by pointer
    ownerTable.add(ownerPtr, map);
  }
  else {
    // index by int name
    ownerTable.add((void const*)(map->intName), map);
  }
}


void BFlatten::xferSerf(void *&serfPtr, bool nullable)
{
  if (writing()) {
    xassert(nullable || serfPtr!=NULL);

    if (serfPtr == NULL) {
      // encode as 0; the names start with 1
      writeInt(0);
    }
    else {
      // lookup the mapping
      OwnerMapping *map = ownerTable.get(serfPtr);

      // we must have already written the owner pointer
      xassert(map != NULL);

      // write the int name
      writeInt(map->intName);
    }
  }
  else /*reading*/ {
    // read the int name
    int name = readInt();

    if (name == 0) {      // null
      xassert(nullable);
      serfPtr = NULL;
    }
    else {
      // lookup the mapping
      OwnerMapping *map = ownerTable.get((void const*)name);
      formatAssert(map != NULL);

      // return the pointer
      serfPtr = map->ownerPtr;
    }
  }
}


// ------------------------ test code ---------------------
#ifdef TEST_BFLATTEN

#include "sm_test.h"

void entry()
{
  // make up some data
  int x = 9, y = 22;
  sm_string s("foo bar");
  int *px = &x, *py = &y;

  // open a file for writing them
  {
    BFlatten flat("bflat.tmp", false /*reading*/);
    flat.xferInt(x);
    flat.noteOwner(&x);
    s.xfer(flat);
    flat.xferSerf((void*&)px);
    flat.xferInt(y);
    flat.noteOwner(&y);
    flat.xferSerf((void*&)py);
  }

  // place to put the data we read
  int x2, y2;
  sm_string s2;
  int *px2, *py2;

  // read them back
  {
    BFlatten flat("bflat.tmp", true /*reading*/);
    flat.xferInt(x2);
    flat.noteOwner(&x2);
    s2.xfer(flat);
    flat.xferSerf((void*&)px2);
    flat.xferInt(y2);
    flat.noteOwner(&y2);
    flat.xferSerf((void*&)py2);
  }

  // compare
  xassert(x == x2);
  xassert(y == y2);
  xassert(s.equals(s2));
  xassert(px2 == &x2);
  xassert(py2 == &y2);

  // delete the temp file
  remove("bflat.tmp");

  printf("bflatten works\n");
}


USUAL_MAIN


#endif // TEST_BFLATTEN
