// xassert.h            see license.txt for copyright and terms of use
// replacement for assert that throws an exception on failure
// (x_assert_fail is defined in exc.cpp)
// Scott McPeak, 1997-1998  This file is public domain.

#ifndef XASSERT_H
#define XASSERT_H

#include "sm_macros.h"

// linkdepend: exc.cpp

void x_assert_fail(char const *cond, char const *file, int line) NORETURN;

// Ordinary 'xassert' *can* be turned off, but the nominal intent
// is that it be left on, under the "ship what you test" theory.
// I advocate using NDEBUG_NO_ASSERTIONS only as a way to gauge the
// performance impact of the existing assertions.
#if !defined(NDEBUG_NO_ASSERTIONS)
  #define xassert(cond) \
    ((cond)? (void)0 : x_assert_fail(#cond, __FILE__, __LINE__))
#else
  #define xassert(cond) ((void)0)
#endif

// Here's a version which will turn off with ordinary NDEBUG.  It
// is for more expensive checks that need not ship.
#if !defined(NDEBUG)
  #define xassertdb(cond) xassert(cond)
#else
  #define xassertdb(cond) ((void)0)
#endif

// call when state is known to be bad; will *not* return
#define xfailure(why) x_assert_fail(why, __FILE__, __LINE__)


// Quick note: one prominent book on writing code recommends that
// assertions *not* include the failure condition, since the file
// and line number are sufficient, and the condition sm_string uses
// memory.  The problem is that sometimes a compiled binary is
// out of date w/respect to the code, and line numbers move, so
// the condition sm_string provides a good way to find the right
// assertion.


/*
  Why throw an exception after an assertion?

  The standard assert() calls abort() after printing its message.
  This is like throwing an exception all the way to the calling
  process.  This is fine if programs are small.

  But when a program is large enough, it may contain subsystems at
  several layers, such that a higher level module is capable of
  recovering from the failure of a lower level module.  Using abort(),
  one would have to resort to catching signals, which is messy.

  An exception is much nicer to catch, and has the added benefit that
  intermediate layers can catch and rethrow, appending little bits of
  context, if they want to make the message more informative.

  In most of my programs, the 'x_assert' exception is only caught in
  main() (implicitly, by catching 'xBase'), and hence 'xassert' acts
  very much like 'assert'.  But by using 'xassert' consistenty, any
  time I *do* have a large program with recovery, all the lower-level
  modules are all ready to cooperate.

  Speaking of recovery: Be aware that when a module fails an
  assertion, its internal state is most likely inconsistent.  Recovery
  actions need to be fairly conservative about what code gets
  re-entered and state re-used after a failure.  This is no different
  than with 'assert', as a program could have inconsistent state *on
  disk* that gets reactivated upon being restarted, but persistent
  (across process boundaries) inconsistent state is simply less
  common.

*/

#endif // XASSERT_H

