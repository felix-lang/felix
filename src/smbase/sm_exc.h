// exc.h            see license.txt for copyright and terms of use
// exception classes for SafeTP project
// Scott McPeak, 1996-1998  This file is public domain.

#ifndef __EXC_H
#define __EXC_H

#include "sm_breaker.h"
#include "sm_typ.h"
#include "sm_xassert.h"
#include "sm_str.h"
#include <iostream>    // std::ostream

// forward declarations
class ELK_EXTERN sm_stringBuilder;


// by using this macro, the debugger gets a shot before the stack is unwound
#ifdef THROW
#undef THROW
#endif
#define THROW(obj) \
  { breaker(); throw (obj); }


// this function returns true if we're in the process of unwinding the
// stack, and therefore destructors may want to avoid throwing new exceptions;
// for now, may return false positives, but won't return false negatives
bool unwinding();

// inside a catch expression, the unwinding() function needs a tweak; pass
// the caught expression, and this returns whether there any *additional*
// exceptions currently in flight
class xBase;
bool unwinding_other(xBase const &x);

// using unwinding() in destructors to avoid abort()
#define CAUTIOUS_RELAY           \
  catch (xBase &x) {             \
    if (!unwinding_other(x)) {   \
      throw;   /* re-throw */    \
    }                            \
  }


// -------------------- xBase ------------------
// intent is to derive all exception objects from this
class xBase {
  sm_string msg;
    // the human-readable description of the exception

public:
  static bool logExceptions;
    // initially true; when true, we write a record of the thrown exception
    // to clog

  static int creationCount;
    // current # of xBases running about; used to support unrolling()

public:
  xBase(char const *m);    // create exception object with message 'm'
  xBase(xBase const &m);   // copy ctor
  virtual ~xBase();

  char const *why() const
    { return (char const*)msg; }

  void insert(std::ostream &os) const;
  friend std::ostream& operator << (std::ostream &os, xBase const &obj)
    { obj.insert(os); return os; }
    // print why
};

// equivalent to THROW(xBase(msg))
void xbase(char const *msg) NORETURN;


// -------------------- x_assert -----------------------
// thrown by _xassert_fail, declared in xassert.h
// throwing this corresponds to detecting a bug in the program
class x_assert : public xBase {
  sm_string condition;          // text of the failed condition
  sm_string filename;           // name of the source file
  int lineno;                // line number

public:
  x_assert(char const *cond, char const *fname, int line);
  x_assert(x_assert const &obj);
  ~x_assert();

  char const *cond() const { return (char const *)condition; }
  char const *fname() const { return (char const *)filename; }
  int line() const { return lineno; }
};


// ---------------------- xFormat -------------------
// throwing this means a formatting error has been detected
// in some input data; the program cannot process it, but it
// is not a bug in the program
class xFormat : public xBase {
  sm_string condition;          // what is wrong with the input

public:
  xFormat(char const *cond);
  xFormat(xFormat const &obj);
  ~xFormat();

  char const *cond() const { return (char const*)condition; }
};

// compact way to throw an xFormat
void xformat(char const *condition) NORETURN;

// convenient combination of condition and human-readable message
#define checkFormat(cond, message) \
  ((cond)? (void)0 : xformat(message))

// assert-like interface to xFormat
void formatAssert_fail(char const *cond, char const *file, int line) NORETURN;

#define formatAssert(cond) \
  ((cond)? (void)0 : formatAssert_fail(#cond, __FILE__, __LINE__))


// -------------------- XOpen ---------------------
// thrown when we fail to open a file
class XOpen : public xBase {
public:
  sm_string filename;

public:
  XOpen(char const *fname);
  XOpen(XOpen const &obj);
  ~XOpen();
};

void throw_XOpen(char const *fname) NORETURN;


#endif // __EXC_H

