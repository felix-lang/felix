// vptrmap.cc
// code for vptrmap.h

#include "sm_vptrmap.h"
#include "sm_xassert.h"

#include <stddef.h>      // NULL
#include <cstring>      // std::memset


// ------------------ VoidPtrMap -------------------
int VoidPtrMap::lookups = 0;
int VoidPtrMap::probes = 0;


VoidPtrMap::VoidPtrMap()
  : hashTable(NULL),
    tableSize(0),
    tableSizeBits(0),
    numEntries(0),
    iterators(0)
{
  alloc(4);    // 16 entries initially
  empty();
}

VoidPtrMap::~VoidPtrMap()
{
  delete[] hashTable;
}


void VoidPtrMap::alloc(int bits)
{
  tableSizeBits = bits;
  tableSize = 1 << bits;
  hashTable = new Entry[tableSize];
}


inline unsigned VoidPtrMap::hashFunc(unsigned multiplier, unsigned key) const
{
  // see Cormen/Leiserson/Rivest (CLR), section 12.3.2

  // multiply, throwing away the overflow high bits
  unsigned ret = key * multiplier;

  // we want to extract the 'tableSizeBits' most sigificant bits
  ret = ret >> ((sizeof(unsigned)*8) - tableSizeBits);
  ret = ret & (tableSize-1);

  return ret;
}


VoidPtrMap::Entry &VoidPtrMap::findEntry(void const *key) const
{
  xassert(key != NULL);
  lookups++;

  // constants used in the hash functions
  enum {
    // value is  floor(  (sqrt(5)-1)/2 * 2^32  )
    //
    // This is the golden ratio.  CLR says Knuth says it's good.
    CONST1 = 0x9E3779B9U,

    // value is  floor(  (sqrt(3)-1)/2 * 2^32  )
    //
    // Some random website claims irrational constants are good,
    // and I can't find any source (I don't have Knuth..) for
    // another constant, so I just decided to substitute 3 for
    // 5 in the golden ratio formula.  Since I trust this one
    // less, I use it for the less important role (stride).
    CONST2 = 0x5DB3D742U
  };

  // compute first hash function, which gives the starting index
  // for the probe sequence
  unsigned index = hashFunc(CONST1, (SM_RAWADDRESS)key);

  // analyze the first entry now, before computing the second
  // hash function (stride) value
  {
    probes++;
    Entry &e = hashTable[index];
    if (e.key == NULL ||
        e.key == key) {
      return e;
    }
  }

  // compute stride; it has to be odd so that it is relatively
  // prime to the table size (which is a power of 2), so I just
  // turn on the least significant bit
  unsigned stride = hashFunc(CONST2, (SM_RAWADDRESS)key) | 1;

  // uncomment this to experiment with linear hashing; when ITERS2MAX
  // is 10000, I see a small increase in avgprobes when using linear
  // hashing over double hashing
  //unsigned stride = 1;

  // collision; stride over the entries
  for (int i=0; i<tableSize; i++) {
    index = (index + stride) & (tableSize-1);

    probes++;
    Entry &e = hashTable[index];
    if (e.key == NULL ||
        e.key == key) {
      return e;
    }
  }

  // searched all entries with no success; but if this happens,
  // then our load factor must be 1, which violates the invariant
  // that numEntries < tableSize
  xfailure("findEntry traversed all entries");
  return *((Entry*)NULL);     // silence warning
}


void VoidPtrMap::add(void *key, void *value)
{
  xassert(iterators == 0);

  // if load factor would exceed 3/4, expand
  if (numEntries+1 > (tableSize/2 + tableSize/4)) {
    expand();
  }

  Entry &e = findEntry(key);
  if (e.key == NULL) {
    e.key = key;              // new mapping
    numEntries++;
  }
  else {
    xassert(e.key == key);    // update existing mapping
  }
  e.value = value;
}


void VoidPtrMap::expand()
{
  Entry *oldHashTable = hashTable;
  int oldTableSize = tableSize;

  alloc(tableSizeBits + 1);
  empty();

  // re-insert all of the old elements
  for (int i=0; i < oldTableSize; i++) {
    Entry &e = oldHashTable[i];
    if (e.key) {
      add(e.key, e.value);
    }
  }

  delete[] oldHashTable;
}


void VoidPtrMap::empty()
{
  xassert(iterators == 0);

  // establishes invariant that NULL keys have NULL values
  std::memset(hashTable, 0, sizeof(*hashTable) * tableSize);
  numEntries = 0;
}


// ------------------- VoidPtrMap::Iter ------------------
VoidPtrMap::Iter::Iter(VoidPtrMap const &m)
  : map(m),
    index(map.tableSize)
{
  map.iterators++;
  adv();
}

VoidPtrMap::Iter::~Iter()
{
  map.iterators--;
}


void VoidPtrMap::Iter::adv()
{
  xassert(index >= 0);
  index--;
  while (index >= 0 &&
         map.hashTable[index].key == NULL) {
    index--;
  }
}


// ------------------- test code ---------------------
#ifdef TEST_VPTRMAP

#include "sm_test.h"
#include "sm_array.h"
#include "sm_ckheap.h"
#include "sm_ptrmap.h"

#include <stdlib.h>    // rand, qsort
#include <stdio.h>     // printf


class Node {
public:
  int *value;
  bool found;

public:
  Node() {
    value = new int(0);
    found = false;
  }
  ~Node() {
    delete value;
  }
};


int doubleCompar(void const *dp1, void const *dp2)
{
  double d1 = *((double*)dp1);
  double d2 = *((double*)dp2);
  if (d1 < d2) return -1;
  if (d1 > d2) return +1;
  return 0;    // almost never happens
}


void test1()
{
  printf("test1: testing PtrMap\n");

  enum { ITERS1=10, ITERS2MAX=2000 };

  double avgprobes[ITERS1];

  printf("  iter  iters  entries  lookups  probes  avgprobes\n");
  printf("  ----  -----  -------  -------  ------  ---------\n");

  for (int i=0; i < ITERS1; i++) {
    // I actually test PtrMap, the type-safe wrapper on top
    // of VoidPtrMap, so I can test that class too; the casts
    // that I used to need for VoidPtrMap are now protected by
    // this CAST macro
    //#define CAST(something) (something)
    #define CAST(something) /*nothing*/

    PtrMap<Node,int> map;
    ObjArrayStack<Node> stack;

    int iters2 = rand() % ITERS2MAX;
    for (int j=0; j < iters2; j++) {
      int op = rand() % 100;

      if (op <= 40) {         // insert
        Node *n = new Node;
        stack.push(n);
        map.add(n, n->value);
      }

      else if (op <= 80) {    // find exist
        if (stack.isNotEmpty()) {
          Node *n = stack[rand() % stack.length()];
          int *v = CAST(int*)map.get(n);
          xassert(v && v == n->value);

          if (rand() % 10 == 0) {
            // reassign
            delete n->value;
            n->value = new int(0);
            map.add(n, n->value);
          }
        }
      }

      else if (op <= 90) {    // find non-exist
        Node *n = new Node;
        int *v = CAST(int*)map.get(n);
        xassert(!v);
        delete n;
      }

      else if (op <= 100) {   // traverse
        // clear all 'found'
        int k;
        for (k=0; k < stack.length(); k++) {
          stack[k]->found = false;
        }

        // walk via map; should find each one exactly once
        int numFound = 0;
        //VoidPtrMap::Iter iter(map);
        PtrMap<Node,int>::Iter iter(map);
        for (; !iter.isDone(); iter.adv()) {
          Node *n = CAST(Node*)iter.key();
          int *v = CAST(int*)iter.value();

          xassert(v == n->value);
          xassert(n->found == false);
          n->found = true;
          numFound++;
        }

        // check all 'found' (launch all 'zig')
        for (k=0; k < stack.length(); k++) {
          xassert(stack[k]->found == true);
        }
        xassert(numFound == stack.length());
      }
    }

    xassert(map.getNumEntries() == stack.length());
    //     "  iter  iters  entries  lookups  probes  avgprobes"
    avgprobes[i] = ((double)VoidPtrMap::probes) / ((double)VoidPtrMap::lookups);
    printf("  %4d  %5d  %7d  %7d  %6d    %g\n",
           i,
           iters2,
           map.getNumEntries(),
           VoidPtrMap::lookups,
           VoidPtrMap::probes,
           avgprobes[i]);

    VoidPtrMap::probes = 0;
    VoidPtrMap::lookups = 0;
  }

  // compute median of avgprobes
  qsort(avgprobes, ITERS1, sizeof(avgprobes[0]), doubleCompar);
  printf("median avgprobe: %g\n", avgprobes[ITERS1/2]);

  //malloc_stats();
}


struct A {
  int x;
  A(int x0) : x(x0) {}
};

void test2()
{
  printf("test2: testing PtrSet\n");

  PtrSet<A> s;
  xassert(s.isEmpty());
  xassert(s.getNumEntries() == 0);

  A *a1 = new A(1);
  s.add(a1);
  xassert(s.isNotEmpty());
  xassert(s.getNumEntries() == 1);

  A *a2 = new A(2);
  s.add(a2);
  xassert(s.isNotEmpty());
  xassert(s.getNumEntries() == 2);

  xassert(s.contains(a1));
  xassert(s.contains(a2));

  s.empty();                    // make empty

  xassert(!s.contains(a1));
  xassert(!s.contains(a2));
  xassert(s.isEmpty());
  xassert(s.getNumEntries() == 0);

  A *a3 = new A(3);
  s.add(a3);
  xassert(s.isNotEmpty());
  xassert(s.getNumEntries() == 1);
}


void entry()
{
  printf("testing vptrmap\n");
  test1();
  test2();
  printf("vptrmap is ok\n");
}


USUAL_MAIN

#endif // TEST_VPTRMAP
