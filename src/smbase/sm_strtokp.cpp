// strtokp.cc            see license.txt for copyright and terms of use
// code for std::strtokp.h
// Scott McPeak, 1997, 1999, 2000  This file is public domain.

#include "sm_strtokp.h"
#include "sm_exc.h"
#include <cstring>     // std::strtok


StrtokParse::StrtokParse(char const *str, char const *delim)
{
  xassert(str != NULL);

  // make local copy
  buf = str;

  // parse it first time to count # of tokens
  int ct=0;
  char *tok = std::strtok(buf.pchar(), delim);
  while (tok) {
    ct++;
    tok = std::strtok(NULL, delim);
  }

  // restore buf
  buf = str;

  // allocate storage
  _tokc = ct;
  if (ct) {
    _tokv = new char*[ct+1];
    _tokv[ct] = NULL;     // terminate argv[]-like list
  }
  else {
    _tokv = NULL;
  }

  // parse it again, this time saving the values
  ct=0;
  tok = std::strtok(buf.pchar(), delim);
  while (tok) {
    _tokv[ct] = tok;
    ct++;
    tok = std::strtok(NULL, delim);
  }

  // simple check just because it's easy
  xassert(ct == _tokc);
}


StrtokParse::~StrtokParse()
{
  // buf deletes itself

  if (_tokv) {
    delete _tokv;
  }
}


void StrtokParse::validate(int which) const
{
  xassert((unsigned)which < (unsigned)_tokc);
}


char const *StrtokParse::tokv(int which) const
{
  validate(which);
  return _tokv[which];
}


sm_string StrtokParse::
  reassemble(int firstTok, int lastTok, char const *original) const
{
  int left = offset(firstTok);
  int right = offset(lastTok) + std::strlen(tokv(lastTok));

  return sm_string(original + left, right-left);
}


sm_string StrtokParse::
  join(int firstTok, int lastTok, char const *separator) const
{
  sm_stringBuilder sb;

  for (int i=firstTok; i<=lastTok; i++) {
    if (i > firstTok) {
      sb << separator;
    }
    sb << tokv(i);
  }

  return sb;
}


int StrtokParse::offset(int which) const
{
  return tokv(which) - (char const*)buf;
}
