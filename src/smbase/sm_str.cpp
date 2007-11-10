// str.cpp            see license.txt for copyright and terms of use
// code for str.h
// Scott McPeak, 1995-2000  This file is public domain.

#include "sm_str.h"

#include <stdlib.h>         // atoi
#include <stdio.h>          // sprintf
#include <ctype.h>          // isspace
#include <cstring>         // std::strcmp
#include <iostream>       // std::ostream << char*
#include <assert.h>         // assert

#include "sm_xassert.h"
#include "sm_ckheap.h"
#include "sm_flatten.h"
#include "sm_nonport.h"


// ----------------------- sm_string ---------------------
// ideally the compiler would arrange for 'empty', and the
// "" it points to, to live in a read-only section of mem..;
// but I can't declare it 'char const *' because I assign
// it to 's' in many places..
char * const sm_string::empty = "";


sm_string::sm_string(char const *src, int length)
{
  s=empty;
  setlength(length);       // setlength already has the +1; sets final NUL
  std::memcpy(s, src, length);
}


void sm_string::dup(char const *src)
{
  if (!src || src[0]==0) {
    s = empty;
  }
  else {
    s = new char[ std::strlen(src) + 1 ];
    xassert(s);
    std::strcpy(s, src);
  }
}

void sm_string::kill()
{
  if (s != empty) {
    delete s;
  }
}


sm_string::sm_string(Flatten&)
  : s(empty)
{}

void sm_string::xfer(Flatten &flat)
{
  flat.xferCharString(s);
}


int sm_string::length() const
{
  xassert(s);
  return std::strlen(s);
}

bool sm_string::contains(char c) const
{
  xassert(s);
  return !!strchr(s, c);
}


sm_string sm_string::subsm_string(int startIndex, int len) const
{
  xassert(startIndex >= 0 &&
          len >= 0 &&
          startIndex + len <= length());

  return sm_string(s+startIndex, len);
}


sm_string &sm_string::setlength(int length)
{
  kill();
  if (length > 0) {
    s = new char[ length+1 ];
    xassert(s);
    s[length] = 0;      // final NUL in expectation of 'length' chars
    s[0] = 0;           // in case we just wanted to set allocated length
  }
  else {
    xassert(length == 0);     // negative wouldn't make sense
    s = empty;
  }
  return *this;
}


int sm_string::compareTo(sm_string const &src) const
{
  return compareTo(src.s);
}

int sm_string::compareTo(char const *src) const
{
  if (src == NULL) {
    src = empty;
  }
  return std::strcmp(s, src);
}


sm_string sm_string::operator&(sm_string const &tail) const
{
  sm_string dest(length() + tail.length());
  std::strcpy(dest.s, s);
  strcat(dest.s, tail.s);
  return dest;
}

sm_string& sm_string::operator&=(sm_string const &tail)
{
  return *this = *this & tail;
}


void sm_string::readdelim(std::istream &is, char const *delim)
{
  sm_stringBuilder sb;
  sb.readdelim(is, delim);
  operator= (sb);
}


void sm_string::write(std::ostream &os) const
{
  os << s;     // standard char* writing routine
}


void sm_string::selfCheck() const
{
  if (s != empty) {
    checkHeapNode(s);
  }
}


// --------------------- sm_stringBuilder ------------------
sm_stringBuilder::sm_stringBuilder(int len)
{
  init(len);
}

void sm_stringBuilder::init(int initSize)
{
  size = initSize + EXTRA_SPACE + 1;     // +1 to be like sm_string::setlength
  s = new char[size];
  end = s;
  end[initSize] = 0;
}


void sm_stringBuilder::dup(char const *str)
{
  int len = std::strlen(str);
  init(len);
  std::strcpy(s, str);
  end += len;
}


sm_stringBuilder::sm_stringBuilder(char const *str)
{
  dup(str);
}


sm_stringBuilder::sm_stringBuilder(char const *str, int len)
{
  init(len);
  std::memcpy(s, str, len);
  end += len;
}


sm_stringBuilder& sm_stringBuilder::operator=(char const *src)
{
  if (s != src) {
    kill();
    dup(src);
  }
  return *this;
}


sm_stringBuilder& sm_stringBuilder::setlength(int newlen)
{
  kill();
  init(newlen);
  return *this;
}


void sm_stringBuilder::adjustend(char* newend)
{
  xassert(s <= newend  &&  newend < s + size);

  end = newend;
  *end = 0;        // sm 9/29/00: maintain invariant
}


sm_stringBuilder& sm_stringBuilder::operator&= (char const *tail)
{
  append(tail, std::strlen(tail));
  return *this;
}

void sm_stringBuilder::append(char const *tail, int len)
{
  ensure(length() + len);

  std::memcpy(end, tail, len);
  end += len;
  *end = 0;
}


sm_stringBuilder& sm_stringBuilder::indent(int amt)
{
  xassert(amt >= 0);
  ensure(length() + amt);

  std::memset(end, ' ', amt);
  end += amt;
  *end = 0;

  return *this;
}


void sm_stringBuilder::grow(int newMinLength)
{
  // I want at least EXTRA_SPACE extra
  int newMinSize = newMinLength + EXTRA_SPACE + 1;         // compute resulting allocated size

  // I want to grow at the rate of at least 50% each time
  int suggest = size * 3 / 2;

  // see which is bigger
  newMinSize = max(newMinSize, suggest);

  // remember old length..
  int len = length();

  // realloc s to be newMinSize bytes
  char *temp = new char[newMinSize];
  xassert(len+1 <= newMinSize);    // prevent overrun
  std::memcpy(temp, s, len+1);          // copy null too
  delete[] s;
  s = temp;

  // adjust other variables
  end = s + len;
  size = newMinSize;
}


sm_stringBuilder& sm_stringBuilder::operator<< (char c)
{
  ensure(length() + 1);
  *(end++) = c;
  *end = 0;
  return *this;
}


#define MAKE_LSHIFT(Argtype, fmt)                        \
  sm_stringBuilder& sm_stringBuilder::operator<< (Argtype arg) \
  {                                                      \
    char buf[60];      /* big enough for all types */    \
    int len = sprintf(buf, fmt, arg);                    \
    if (len >= 60) {                                     \
      abort();    /* too big */                          \
    }                                                    \
    return *this << buf;                                 \
  }

MAKE_LSHIFT(long, "%ld")
MAKE_LSHIFT(unsigned long, "%lu")
MAKE_LSHIFT(float, "%g")
MAKE_LSHIFT(double, "%g")
MAKE_LSHIFT(void*, "%p")
#if FLX_HAVE_LONGLONG
MAKE_LSHIFT(unsigned long long, "%llu")
MAKE_LSHIFT(long long, "%lld")
#endif
#if FLX_HAVE_LONGDOUBLE
MAKE_LSHIFT(long double, "%Lg")
#endif
#undef MAKE_LSHIFT


sm_stringBuilder& sm_stringBuilder::operator<< (
  sm_stringBuilder::Hex const &h)
{
  char buf[32];        // should only need 19 for 64-bit word..
  int len = sprintf(buf, "0x%lX", h.value);
  if (len >= 20) {
    abort();
  }
  return *this << buf;

  // the length check above isn't perfect because we only find out there is
  // a problem *after* trashing the environment.  it is for this reason I
  // use 'assert' instead of 'xassert' -- the former calls abort(), while the
  // latter throws an exception in anticipation of recoverability
}


sm_stringBuilder& sm_stringBuilder::operator<< (Manipulator manip)
{
  return manip(*this);
}


// slow but reliable
void sm_stringBuilder::readdelim(std::istream &is, char const *delim)
{
  char c;
  is.get(c);
  while (!is.eof() &&
         (!delim || !strchr(delim, c))) {
    *this << c;
    is.get(c);
  }
}


// ---------------------- toString ---------------------
#define TOSTRING(type)        \
  sm_string toString(type val)   \
  {                           \
    return sm_stringc << val;    \
  }

TOSTRING(int)
TOSTRING(unsigned)
TOSTRING(char)
TOSTRING(long)
TOSTRING(float)

#undef TOSTRING

// this one is more liberal than 'sm_stringc << null' because it gets
// used by the PRINT_GENERIC macro in my astgen tool
sm_string toString(char const *str)
{
  if (!str) {
    return sm_string("(null)");
  }
  else {
    return sm_string(str);
  }
}


// ------------------- sm_stringf -----------------
sm_string sm_stringf(char const *format, ...)
{
  va_list args;
  va_start(args, format);
  sm_string ret = vsm_stringf(format, args);
  va_end(args);
  return ret;
}


sm_string vsm_stringf(char const *format, va_list args)
{
  // estimate sm_string length
  // int est = vnprintf(format, args);

  va_list args2;
  va_copy(args2, args);
  int est = vnprintf(format, args2);
  va_end(args2);

  // allocate space
  sm_string ret(est+1);

  // render the sm_string
  int len = vsprintf(ret.pchar(), format, args);

  // check the estimate, and fail *hard* if it was low, to avoid any
  // possibility that this might become exploitable in some context
  // (do *not* turn this check off in an NDEGUG build)
  if (len > est) {
    // don't go through fprintf, etc., because the state of memory
    // makes that risky
    static char const msg[] =
      "fatal error: vnprintf failed to provide a conservative estimate,\n"
      "memory is most likely corrupted\n";
    fprintf(stderr, msg);
    abort();
  }

  // happy
  return ret;
}


// ------------------ test code --------------------
#ifdef TEST_STR

#include <iostream>    // std::cout

void test(unsigned long val)
{
  //std::cout << sm_stringb(val << " in hex: 0x" << sm_stringBuilder::Hex(val)) << std::endl;

  std::cout << sm_stringb(val << " in hex: " << SBHex(val)) << std::endl;
}

int main()
{
  // for the moment I just want to test the hex formatting
  test(64);
  test(0xFFFFFFFF);
  test(0);
  test((unsigned long)(-1));
  test(1);

  std::cout << "sm_stringf: " << sm_stringf("int=%d hex=%X str=%s char=%c float=%f",
                                 5, 0xAA, "hi", 'f', 3.4) << std::endl;

  std::cout << "tests passed\n";

  return 0;
}

#endif // TEST_STR

