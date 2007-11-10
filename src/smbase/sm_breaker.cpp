// breaker.cc            see license.txt for copyright and terms of use
// code for breaker.h
// Scott McPeak, 1997,1998  This file is public domain.

#include "sm_breaker.h"

#ifdef __BORLANDC__
# pragma warn -use
#endif

void ackackack(int*) {}

void breaker()
{
  static int i=0;
  int a=1;               // all this junk is just to make sure
                         // that this function has a complete
  ackackack(&a);         // stack frame, so the debugger can unwind
  i++;                   // the stack
}

#ifdef __BORLANDC__
#  pragma warn .use
#endif

// (tweak for CVS)
