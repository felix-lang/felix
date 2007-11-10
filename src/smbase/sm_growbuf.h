// growbuf.h            see license.txt for copyright and terms of use
// buffer that grows as needed by doubling in size

#ifndef __GROWBUF_H
#define __GROWBUF_H

#include "sm_datablok.h"

class GrowBuffer : public DataBlock {
public:
  GrowBuffer(int allocSize=0)
    : DataBlock(allocSize) {}
  ~GrowBuffer() {}

  // append to the end, at least doubling allocated
  // size if growth is needed
  void append(byte const *str, int len);
  void append(char const *str, int len)
    { append((byte const*)str, len); }
};

#endif // __GROWBUF_H
