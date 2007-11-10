// bitarray.h            see license.txt for copyright and terms of use
// one-dimensional array of bits

#ifndef BITARRAY_H
#define BITARRAY_H

#include "sm_xassert.h"

class Flatten;            // flatten.h

class BitArray {
private:    // data
  unsigned char *bits;
  int numBits;              // # of bits in the array

private:    // disallowed for now
  BitArray(BitArray&);
  void operator=(BitArray&);

private:    // funcs
  void bc(int i) const { xassert((unsigned)i < (unsigned)numBits); }
  int allocdBytes() const { return (numBits+7) / 8; }

public:     // funcs
  BitArray(int n);          // create with given # of bits, initially zeroed
  ~BitArray();

  BitArray(Flatten&);
  void xfer(Flatten &flat);

  // test a bit, return 0 or 1
  int test(int i) const
    { bc(i); return ((bits[i >> 3]) >> (i & 7)) & 1; }

  // set a bit to a specific value
  void set(int i)
    { bc(i); bits[i >> 3] |= (1 << (i & 7)); }
  void reset(int i)
    { bc(i); bits[i >> 3] &= ~(1 << (i & 7)); }

  // set a bit to an arbitrary value
  void setTo(int i, int val) {
    if (val) {
      set(i);
    }
    else {
      reset(i);
    }
  }

  // clear all bits
  void clearAll();
};


#endif // BITARRAY_H
