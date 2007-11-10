// malloc_stub.h            see license.txt for copyright and terms of use
// no-op implementation of ckheap.h

#include "sm_ckheap.h"     // interface to implement

void checkHeap() {}

void checkHeapNode(void *node) {}

void malloc_stats() {}

unsigned numMallocCalls() { return 0; }
unsigned numFreeCalls() { return 0; }

void walkMallocHeap(HeapWalkFn func) {}

// EOF
