// bitarray.cc            see license.txt for copyright and terms of use
// code for bitarray.h

#include "sm_bitarray.h"
#include "sm_flatten.h"

#include <cstring>       // std::memset


BitArray::BitArray(int n)
  : numBits(n)
{
  bits = new unsigned char[allocdBytes()];
  clearAll();
}


BitArray::~BitArray()
{
  delete[] bits;
}


BitArray::BitArray(Flatten&)
  : bits(NULL)
{}

void BitArray::xfer(Flatten &flat)
{
  flat.xferInt(numBits);

  if (flat.reading()) {
    bits = new unsigned char[allocdBytes()];
  }
  flat.xferSimple(bits, allocdBytes());
}


void BitArray::clearAll()
{
  std::memset(bits, 0, allocdBytes());
}

