// ckheap.h            see license.txt for copyright and terms of use
// interface to check heap integrity, etc.

#ifndef CKHEAP_H
#define CKHEAP_H

#ifdef __cplusplus
extern "C" {
#endif


// check heap integrity, and fail an assertion if it's bad
void checkHeap();

// check that a given pointer is a valid allocated object;
// fail assertion if not
void checkHeapNode(void *node);

// prints allocation statistics to stderr
void malloc_stats();

// count # of malloc/free calls in program
unsigned numMallocCalls();
unsigned numFreeCalls();


// actions the heap walk iterator might request
enum HeapWalkOpts {
  HW_GO   = 0,         // keep going
  HW_STOP = 1,         // stop iterating
  HW_FREE = 2,         // free the block I just examined
};

// function for walking the heap
//   block:   pointer to the malloc'd block of memory
//   size:    # of bytes in the block; possibly larger than
//            what was requested
//   returns: bitwise OR of HeapWalkOpts options
// NOTE: you cannot call malloc or free inside this function
// (you can cause 'block' to be freed by returning HW_FREE)
typedef enum HeapWalkOpts (*HeapWalkFn)(void *block, int size);

// heap walk entry
void walkMallocHeap(HeapWalkFn func);


#ifdef __cplusplus
} // extern "C"
#endif

#endif // CKHEAP_H
