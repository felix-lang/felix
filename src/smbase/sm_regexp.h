// smregexp.h
// regular expression matching, etc.

// the "sm" prefix in the name is to avoid a name conflict with something
// in my version of glibc..

// The regular expression language is, for now, the "Extended" regexp
// language described in the regex(7) man page, itself a description
// of POSIX 1003.2, section 2.8 (Regular Expression Notation).

// The interface is based on the POSIX regex functions too, but I
// don't actually include regex.h here since I want to allow a
// different implementation, if that becomes necessary.

#ifndef REGEXP_H
#define REGEXP_H

#include "sm_macros.h"


// ----------------- Regexp class -------------------
// This class represents a compiled regexp pattern.  For maximum
// efficiency, repeated uses of the same pattern should use the
// same Regexp object each time instead of making a new one.
class Regexp {
public:      // types
  // compilation flags
  enum CFlags {
    C_NONE     = 0x00,         // no flags
    ICASE      = 0x02,         // case insensitive
    NOSUB      = 0x08,         // subsm_string matches are not needed
    //NEWLINE                  // still not sure what this really means
  };

  // execution flags
  enum EFlags {
    E_NONE     = 0x00,         // no flags
    NOTBOL     = 0x01,         // treat 'str' as not including beginning of line
    NOTEOL     = 0x02,         // ............................ end of line
  };

private:     // data
  void *impl;                  // implementation data
  //int numLParens;              // # of left-parens in the pattern

private:     // funcs
  // not allowed
  Regexp(Regexp&);
  void operator=(Regexp&);

  void err(int code) NORETURN;

public:      // funcs
  Regexp(char const *exp, CFlags flags = C_NONE);
  ~Regexp();

  bool match(char const *str, EFlags flags = E_NONE);
};

// allow '|' to be used on the flags
ENUM_BITWISE_OR(Regexp::CFlags)
ENUM_BITWISE_OR(Regexp::EFlags)


// TODO: Add support for subsm_string matches by building a class to
// remember the subsm_string offsets (enable 'numLParens' above)
// efficiently.  Major question: do I always make an internal copy of
// the sm_string in which we searched?  Leaning towards yes...



// --------------- convenience functions ---------------
// The functions in this section are built on top of the
// Regexp class in the obvious way.

// return true if 'str' matches 'exp'
bool regexpMatch(char const *str, char const *exp);


#endif // REGEXP_H
