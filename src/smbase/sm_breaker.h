// breaker.h            see license.txt for copyright and terms of use
// function stub through which critical event code flow is directed
//   for easy breakpoints
// Scott McPeak, 1997,1998  This file is public domain.

#ifndef __BREAKER_H
#define __BREAKER_H

void breaker();

// bassert = breaker assert; failure simply calls breaker, which is
// a breakpoint in the debugger and is ignored when not in debugger;
// useful mainly for places I want to ensure something is true during
// initial testing, but after that it's ok if it's false
template <class T>          // allow possibly null pointers, etc
inline void bassert(T cond)
{
  if (!cond) {
    breaker();
  }
}


// this will call breaker on the first pass, but not any subsequent (unless
// it's called MAXINT*2 times...)
#define BREAK_FIRST_PASS     \
  {                          \
    static int passCount=0;  \
    bassert(passCount++);    \
  } /*no semicolon*/


// this is obsolete...
void _breaker_assert(char * __cond, char * __file, int __line);
  // this will be called on failed assertions instead of _assert
  // only if BREAKER_ASSERT is defined (due to a modification to
  // assert.h directly)

#endif // __BREAKER_H

