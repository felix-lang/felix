// smregexp.cc
// code for smregexp.h

#include "sm_regexp.h"
#include "sm_str.h"
#include "sm_exc.h"

#include <stddef.h>       // size_t

// for now, I implement everything using the libc POSIX regex
// facilities
//
// linux (etc.) has proper declarations in regex.h, but FreeBSD (and
// other BSDs?) has regex.h contents that do not compile under C++,
// and apparently gnuregex.h is the substitute that does
#ifndef __FreeBSD__
  #include <regex.h>
#else
  #include <gnuregex.h>
#endif


// get an error sm_string
static sm_string regexpErrorString(regex_t const *pat, int code)
{
  // find out how long the error sm_string is; this size
  // includes the final NUL byte
  int size = regerror(code, pat, NULL, 0);

  // get the sm_string
  sm_string ret(size);
  regerror(code, pat, ret.pchar(), size);

  return ret;
}

// throw an exception
static void regexpError(regex_t const *pat, int code) NORETURN;
static void regexpError(regex_t const *pat, int code)
{
  xbase(regexpErrorString(pat, code));
}


// -------------------- Regexp --------------------------
// interpretation of 'impl' field
#define PAT ((regex_t*&)impl)

Regexp::Regexp(char const *exp, CFlags flags)
{
  PAT = new regex_t;

  int f = REG_EXTENDED;    // "extended" language

  // if the values I chose line up perfectly with the values used by
  // libc, then I don't have to interpret them (hopefully the
  // optimizer will discover that the 'if' test is constant
  // (gcc-2.95.3's optimizer does); I can't do it with the
  // preprocessor because it can't see the enumerator values)
  if (REG_ICASE==ICASE && REG_NOSUB==NOSUB) {
    f |= (int)flags;
  }
  else {
    // interpret my flags
    if (flags & ICASE) f |= REG_ICASE;
    if (flags & NOSUB) f |= REG_NOSUB;
  }

  int code = regcomp(PAT, exp, f);
  if (code) {
    // deallocate the pattern buffer before throwing the exception
    sm_string msg = regexpErrorString(PAT, code);
    delete PAT;
    xbase(msg);
  }
}

Regexp::~Regexp()
{
  regfree(PAT);
  delete PAT;
}


void Regexp::err(int code)
{
  regexpError(PAT, code);
}


bool Regexp::match(char const *str, EFlags flags)
{
  int f = 0;

  // same thing as above
  if (REG_NOTBOL==NOTBOL && REG_NOTEOL==NOTEOL) {
    f = (int)flags;
  }
  else {
    if (flags & NOTBOL) f |= REG_NOTBOL;
    if (flags & NOTEOL) f |= REG_NOTEOL;
  }

  int code = regexec(PAT, str, 0, NULL, f);
  if (code == 0) {
    return true;
  }
  else if (code == REG_NOMATCH) {
    return false;
  }
  else {
    err(code);
  }
}


#undef PAT


// --------------- convenience functions ---------------
bool regexpMatch(char const *str, char const *exp)
{
  Regexp pat(exp, Regexp::NOSUB);
  return pat.match(str);
}


// ----------------- test code --------------------
#ifdef TEST_SMREGEXP

#include <stdlib.h>    // exit
#include <stdio.h>     // printf


void matchVector(char const *str, char const *exp, bool expect)
{
  bool actual = regexpMatch(str, exp);
  if (actual != expect) {
    printf("regexp failure\n");
    printf("  str: %s\n", str);
    printf("  exp: %s\n", exp);
    printf("  expect: %s\n", (expect? "true" : "false"));
    printf("  actual: %s\n", (actual? "true" : "false"));
    exit(2);
  }
}


int main()
{
  matchVector("abc", "a", true);
  matchVector("abc", "b", true);
  matchVector("abc", "c", true);
  matchVector("abc", "d", false);

  matchVector("abc", "^a", true);
  matchVector("abc", "^b", false);
  matchVector("abc", "b$", false);
  matchVector("abc", "c$", true);
  matchVector("abc", "^d", false);

  printf("regexp works\n");
  return 0;
}


#endif // TEST_SMREGEXP
