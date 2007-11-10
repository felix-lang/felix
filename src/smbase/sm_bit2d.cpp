// bit2d.cc            see license.txt for copyright and terms of use
// code for bit2d.h

#include "sm_bit2d.h"
#include "sm_xassert.h"
#include "sm_flatten.h"

#include <cstring>     // std::memset, std::memcpy
#include <stdio.h>      // printf


Bit2d::Bit2d(point const &aSize)
  : owning(true),
    size(aSize)
{
  xassert(size.x > 0 && size.y > 0);
  stride = (size.x+7)/8;
  data = new byte[datasize()];
}


Bit2d::~Bit2d()
{
  if (owning) {
    delete data;
  }
}


Bit2d::Bit2d(Bit2d const &obj)
{
  size = obj.size;
  stride = obj.stride;
  data = new byte[datasize()];
  owning = true;
  std::memcpy(data, obj.data, datasize());
}


Bit2d& Bit2d::operator= (Bit2d const &obj)
{
  if (this != &obj) {
    xassert(size == obj.size);
    std::memcpy(data, obj.data, datasize());
  }
  return *this;
}


bool Bit2d::operator== (Bit2d const &obj) const
{
  return (size == obj.size) &&
         (0==std::memcmp(data, obj.data, datasize()));
}


Bit2d::Bit2d(Flatten &)
  : data(NULL),
    owning(true)
{}

void Bit2d::xfer(Flatten &flat)
{
  flat.xferInt(size.x);
  flat.xferInt(size.y);
  flat.xferInt(stride);

  flat.xferHeapBuffer((void*&)data, datasize());
}


void Bit2d::setall(int val)
{
  std::memset(data, val? 0xFF : 0, datasize());
}


int Bit2d::get(point const &p) const
{
  xassert(okpt(p));
  return ( *(byteptrc(p)) >> (p.x&7) ) & 1;
}

void Bit2d::set(point const &p)
{
  xassert(okpt(p));
  *(byteptr(p)) |= (byte)  ( 1 << (p.x&7) ) ;
}

void Bit2d::reset(point const &p)
{
  xassert(okpt(p));
  *(byteptr(p)) &= (byte)(~( 1 << (p.x&7) ));
}

void Bit2d::setto(point const &p, int val)
{
  if (val) { set(p); }
  else { reset(p); }
}

int Bit2d::testAndSet(point const &p)
{
  byte *b = byteptr(p);
  int ret = (*b >> (p.x&7)) & 1;
  *b |= (byte)( 1 << (p.x&7) );
  return ret;
}

void Bit2d::toggle(point const &p)
{
  xassert(okpt(p));
  *(byteptr(p)) ^= (byte) ( 1 << (p.x&7) );
}


// count the number of digits required to represent a positive
// integer in base 10
static int digits(int value)
{
  xassert(value > 0);
  int ct=0;
  while (value > 0) {
    ct++;
    value /= 10;
  }
  return ct;
}


/*
 * Goal is to draw something like this:
 *
 *       1  2  3
 *  1 [  0  0  0  ]
 *  2 [  0  1  1  ]
 *  3 [  0  1  0  ]
 *
 */
void Bit2d::print() const
{
  // compute column widths
  int rowLabelWidth = digits(size.y-1);
  int colLabelWidth = digits(size.x-1);

  // column legend
  printf("%*s   ", rowLabelWidth, "");
  loopi(size.x) {
    printf("%*d ", colLabelWidth, i);
  }
  printf("\n");

  for (int row=0; row<size.y; row++) {
    printf("%*d [ ", rowLabelWidth, row);
    loopi(size.x) {
      printf("%*s ", colLabelWidth,
                     get(point(i, row))? "1" : ".");    // "." so easier to see patterns
    }
    printf("]\n");
  }
}


// hack
Bit2d::Bit2d(byte * /*serf*/ d, point const &sz, int str)
  : data(d),
    owning(false),    // since it's a serf ptr
    size(sz),
    stride(str)
{}



// ------------------------ test code ------------------------
#ifdef TEST_BIT2D

#include "sm_bflatten.h"

int main()
{
  Bit2d bits(point(17,3));
  xassert(bits.okpt(point(16,2)) &&
         !bits.okpt(point(17,3)) &&
         !bits.okpt(point(2,16)));

  bits.setall(0);
  xassert(!bits.testAndSet(point(9,1)));
  xassert(bits.testAndSet(point(9,1)));

  xassert(!bits.testAndSet(point(2,0)));
  xassert(bits.testAndSet(point(2,0)));

  xassert(!bits.testAndSet(point(16,2)));
  xassert(bits.testAndSet(point(16,2)));

  bits.toggle(point(3,2));
  xassert(bits.get(point(3,2)));

  bits.print();

  // test read/write
  Bit2d *another = writeThenRead(bits);
  xassert(*another == bits);
  delete another;

  printf("bit2d works\n");

  return 0;
}

#endif // TEST_BIT2D

