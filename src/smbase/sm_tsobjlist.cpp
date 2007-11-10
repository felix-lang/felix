// tsobjlist.cc            see license.txt for copyright and terms of use
// test of sobjlist.h

#include "sm_sobjlist.h"
#include <stdio.h>       // printf

int main()
{
  char const *hi = "hi there";
  char const *what = "what's up?";

  // the real purpose of this test is to make sure it's ok to
  // add a 'const' qualifier inside the angle brackets, and get
  // the effect I'm after
  SObjList<char const> list;

  // 'prepend' accepts a T*, which should become a char const *;
  // if it only becomes (e.g.) a char*, then this call should
  // trigger a compile error
  list.prepend(hi);

  list.append(what);

  // 'indexOf' accepts a T const *, so here I'm essentially verifying
  // the compiler doesn't mind seeing 'const' twice
  int i = list.indexOf(hi);
  printf("index of 'hi' is %d\n", i);

  i = list.indexOf(what);
  printf("index of 'what' is %d\n", i);

  // random test of extra 'const' outside the template context
  // (gcc-2.95.3 doesn't like it, interesting..)
  int const /*const*/ x = 5;
  printf("x is %d\n", x);

  return 0;
}

// EOF
