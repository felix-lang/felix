// str.h            see license.txt for copyright and terms of use
// a sm_string class
// the representation uses just one char*, so that a smart compiler
//   can pass the entire object as a single word
// Scott McPeak, 1995-2000  This file is public domain.

#ifndef __FLX_SM_STR_H__
#define __FLX_SM_STR_H__

// this should eventually be put someplace more general...
#ifndef va_copy
  #ifdef __va_copy
    #define va_copy(a,b) __va_copy(a,b)
  #else
    #define va_copy(a,b) (a)=(b)
  #endif
#endif
// MOVE THE ABOVE TO PLACE MORE GENERAL

#include "flx_elk_config.hpp"
#include "sm_typ.h"
#include <iostream>    // std::istream, std::ostream
#include <stdarg.h>      // va_list

class Flatten;           // flatten.h
class ELK_EXTERN sm_string;

class ELK_EXTERN sm_string {
protected:     // data
  // 10/12/00: switching to never letting s be NULL
  char *s;                             // sm_string contents; never NULL
  static char * const empty;           // a global ""; should never be modified

protected:     // funcs
  void dup(char const *source);        // copies, doesn't dealloc first
  void kill();                         // dealloc if str != 0

public:        // funcs
  sm_string(sm_string const &src) { dup(src.s); }
  sm_string(char const *src) { dup(src); }
  sm_string(char const *src, int length);    // grab a subsm_string
  sm_string(int length) { s=empty; setlength(length); }
  sm_string() { s=empty; }

  ~sm_string() { kill(); }

  sm_string(Flatten&);
  void xfer(Flatten &flat);

  // simple queries
  int length() const;           // returns number of non-null chars in the sm_string; length of "" is 0
  bool isempty() const { return s[0]==0; }
  bool contains(char c) const;

  // array-like access
  char& operator[] (int i) { return s[i]; }
  char operator[] (int i) const { return s[i]; }

  // subsm_string
  sm_string subsm_string(int startIndex, int length) const;

  // conversions
  //operator char* () { return s; }      // ambiguities...
  operator char const* () const { return s; }
  char *pchar() { return s; }
  char const *pcharc() const { return s; }

  // assignment
  sm_string& operator=(sm_string const &src)
    { if (&src != this) { kill(); dup(src.s); } return *this; }
  sm_string& operator=(char const *src)
    { if (src != s) { kill(); dup(src); } return *this; }

  // allocate 'newlen' + 1 bytes (for null); initial contents is ""
  sm_string& setlength(int newlen);

  // comparison; return value has same meaning as strcmp's return value:
  //   <0   if   *this < src
  //   0    if   *this == src
  //   >0   if   *this > src
  int compareTo(sm_string const &src) const;
  int compareTo(char const *src) const;
  bool equals(char const *src) const { return compareTo(src) == 0; }

  #define MAKEOP(op)                                                             \
    bool operator op (sm_string const &src) const { return compareTo(src) op 0; }   \
    /*bool operator op (const char *src) const { return compareTo(src) op 0; }*/ \
    /* killed stuff with char* because compilers are too flaky; use compareTo */
  MAKEOP(==)  MAKEOP(!=)
  MAKEOP(>=)  MAKEOP(>)
  MAKEOP(<=)  MAKEOP(<)
  #undef MAKEOP

  // concatenation (properly handles sm_string growth)
  // uses '&' instead of '+' to avoid char* coercion problems
  sm_string operator& (sm_string const &tail) const;
  sm_string& operator&= (sm_string const &tail);

  // input/output
  friend std::istream& operator>> (std::istream &is, sm_string &obj)
    { obj.readline(is); return is; }
  friend std::ostream& operator<< (std::ostream &os, sm_string const &obj)
    { obj.write(os); return os; }

  // note: the read* functions are currently implemented in a fairly
  // inefficient manner (one char at a time)

  void readdelim(std::istream &is, char const *delim);
    // read from is until any character in delim is encountered; consumes that
    // character, but does not put it into the sm_string; if delim is null or
    // empty, reads until EOF

  void readall(std::istream &is) { readdelim(is, NULL); }
    // read all remaining chars of is into this

  void readline(std::istream &is) { readdelim(is, "\n"); }
    // read a line from input stream; consumes the \n, but doesn't put it into
    // the sm_string

  void write(std::ostream &os) const;
    // writes all stored characters (but not '\0')

  // debugging
  void selfCheck() const;
    // fail an assertion if there is a problem
};


// replace() and trimWhiteSpace() have been moved to strutil.h


// this class is specifically for appending lots of things
class ELK_EXTERN sm_stringBuilder : public sm_string {
protected:
  enum { EXTRA_SPACE = 30 };    // extra space allocated in some situations
  char *end;          // current end of the sm_string (points to the NUL character)
  int size;           // amount of space (in bytes) allocated starting at 's'

protected:
  void init(int initSize);
  void dup(char const *src);

public:
  sm_stringBuilder(int length=0);    // creates an empty sm_string
  sm_stringBuilder(char const *str);
  sm_stringBuilder(char const *str, int length);
  sm_stringBuilder(sm_string const &str) { dup((char const*)str); }
  sm_stringBuilder(sm_stringBuilder const &obj) { dup((char const*)obj); }
  ~sm_stringBuilder() {}

  sm_stringBuilder& operator= (char const *src);
  sm_stringBuilder& operator= (sm_string const &s) { return operator= ((char const*)s); }
  sm_stringBuilder& operator= (sm_stringBuilder const &s) { return operator= ((char const*)s); }

  int length() const { return end-s; }
  bool isempty() const { return length()==0; }

  sm_stringBuilder& setlength(int newlen);    // change length, forget current data

  // make sure we can store 'someLength' non-null chars; grow if necessary
  void ensure(int someLength) { if (someLength >= size) { grow(someLength); } }

  // grow the sm_string's length (retaining data); make sure it can hold at least
  // 'newMinLength' non-null chars
  void grow(int newMinLength);

  // this can be useful if you modify the sm_string contents directly..
  // it's not really the intent of this class, though
  void adjustend(char* newend);

  // make the sm_string be the empty sm_string, but don't change the
  // allocated space
  void clear() { adjustend(s); }

  // concatenation, which is the purpose of this class
  sm_stringBuilder& operator&= (char const *tail);

  // useful for appending subsm_strings or sm_strings with NUL in them
  void append(char const *tail, int length);

  // append a given number of spaces; meant for contexts where we're
  // building a multi-line sm_string; returns '*this'
  sm_stringBuilder& indent(int amt);

  // sort of a mixture of Java compositing and C++ i/o strstream
  // (need the coercion versions (like int) because otherwise gcc
  // spews mountains of f-ing useless warnings)
  sm_stringBuilder& operator << (char const *text) { return operator&=(text); }
  sm_stringBuilder& operator << (char c);
  sm_stringBuilder& operator << (unsigned char c) { return operator<<((char)c); }
  sm_stringBuilder& operator << (long i);
  sm_stringBuilder& operator << (unsigned long i);
  sm_stringBuilder& operator << (int i) { return operator<<((long)i); }
  sm_stringBuilder& operator << (unsigned i) { return operator<<((unsigned long)i); }
  sm_stringBuilder& operator << (short i) { return operator<<((long)i); }
  sm_stringBuilder& operator << (unsigned short i) { return operator<<((long)i); }
  sm_stringBuilder& operator << (float d);
  sm_stringBuilder& operator << (double d);
  sm_stringBuilder& operator << (const void *ptr);     // inserts address in hex
#if FLX_HAVE_LONGLONG
  sm_stringBuilder& operator << (long long i);
  sm_stringBuilder& operator << (unsigned long long i);
#endif
#if FLX_HAVE_LONGDOUBLE
  sm_stringBuilder& operator << (long double d);
#endif
#if FLX_HAVE_BOOL
  sm_stringBuilder& operator << (bool b) { return operator<<((long)b); }
#endif

  // useful in places where long << expressions make it hard to
  // know when arguments will be evaluated, but order does matter
  typedef sm_stringBuilder& (*Manipulator)(sm_stringBuilder &sb);
  sm_stringBuilder& operator<< (Manipulator manip);

  // stream readers
  friend std::istream& operator>> (std::istream &is, sm_stringBuilder &sb)
    { sb.readline(is); return is; }
  void readall(std::istream &is) { readdelim(is, NULL); }
  void readline(std::istream &is) { readdelim(is, "\n"); }

  void readdelim(std::istream &is, char const *delim);

  // an experiment: hex formatting (something I've sometimes done by resorting
  // to sprintf in the past)
  class Hex {
  public:
    unsigned long value;

    Hex(unsigned long v) : value(v) {}
    Hex(Hex const &obj) : value(obj.value) {}
  };
  sm_stringBuilder& operator<< (Hex const &h);
  #define SBHex sm_stringBuilder::Hex
};


// the real strength of this entire module: construct sm_strings in-place
// using the same syntax as C++ istd::ostreams.  e.g.:
//   puts(sm_stringb("x=" << x << ", y=" << y));
#define sm_stringb(expr) (sm_stringBuilder() << expr)

// experimenting with dropping the () in favor of <<
// (the "c" can be interpreted as "constructor", or maybe just
// the successor to "b" above)
#define sm_stringc sm_stringBuilder()


// experimenting with using toString as a general method for datatypes
sm_string toString(int i);
sm_string toString(unsigned i);
sm_string toString(char c);
sm_string toString(long i);
sm_string toString(char const *str);
sm_string toString(float f);


// printf-like construction of a sm_string; often very convenient, since
// you can use any of the formatting characters (like %X) that your
// libc's sprintf knows about
sm_string sm_stringf(char const *format, ...);
sm_string vsm_stringf(char const *format, va_list args);


#endif // STR_H
