// strtokp.h            see license.txt for copyright and terms of use
// using std::strtok to parse an entire sm_string at once
// Scott McPeak, 1997  This file is public domain.

#ifndef __STRTOKP_H
#define __STRTOKP_H

#include "sm_str.h"
#include <cstring>    // std::strlen

class StrtokParse {
  sm_string buf;          // locally allocated storage
  int _tokc;           // # of tokens found
  char **_tokv;        // array of tokens themselves

private:
  void validate(int which) const;
    // throw an exception if which is invalid token

public:
  StrtokParse(char const *str, char const *delim);
    // parse 'str' into tokens delimited by chars from 'delim'

  ~StrtokParse();
    // clean up'

  int tokc() const { return _tokc; }
  operator int () const { return tokc(); }
    // simple count of available tokens

  char const *tokv(int which) const;     // may throw xArrayBounds
  char const* operator[] (int which) const { return tokv(which); }
    // access to tokens; must make local copies to modify

  sm_string reassemble(int firstTok, int lastTok, char const *originalString) const;
    // return the subsm_string of the original sm_string spanned by the
    // given range of tokens; if firstTok==lastTok, only that token is
    // returned (without any separators); must be that firstTok <=
    // lastTok

  sm_string join(int firstTok, int lastTok, char const *separator) const;
    // return a sm_string created by concatenating the given range of tokens
    // together with 'separator' in between them

  int offset(int which) const;
    // return a value that, when added to the original 'str' parameter,
    // yields a pointer to where tokv(which) is, as a subsm_string, in that sm_string

  int offsetAfter(int which) const
    { return offset(which) + std::strlen(tokv(which)); }
    // offset for character just beyond last one in tokv (should be either
    // a delimiter character, or 0)

  char **spawn_tokv_array() { return _tokv; }
    // this is defined because it makes it convenient to generate
    // spawn arguments, and should be limited to use for that purpose
    // (because it exposes internal representation which is in
    // principle subject to change)
};

#endif // __STRTOKP_H

