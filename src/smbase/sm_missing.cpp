// missing.cc            see license.txt for copyright and terms of use
// code for missing.h
// Scott McPeak, 1998  This file is public domain.

#include "sm_missing.h"

#include <ctype.h>       // tolower

int missing_stricmp(char const *s1, char const *s2)
{
  while (*s1 && *s2) {
    // the choice between tolower and toupper affects lexicographic
    // comparisons between letters and the symbols between Z and a;
    // I don't know which is the "correct" way.
    int d = tolower(*s1) - tolower(*s2);
    if (d != 0) {
      return d;
    }
    s1++;
    s2++;
  }

  // one or both are at the null terminator
  return *s1 - *s2;
}
