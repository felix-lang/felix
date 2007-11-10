// ccsstr.h            see license.txt for copyright and terms of use
// C++ substrate for my parser

#ifndef CCSSTR_H
#define CCSSTR_H

#include "ast_embedded.h"

class CCSubstrateTest;

class CCSubstrate : public EmbeddedLang {
private:
  enum State {
    ST_NORMAL,       // normal text
    ST_STRING,       // inside a sm_string literal
    ST_CHAR,         // inside a char literal
    ST_SLASH,        // from ST_NORMAL, just saw a slash
    ST_C_COMMENT,    // inside a C comment
    ST_CC_COMMENT,   // inside a C++ comment
    NUM_STATES
  } state;
  int nesting;       // depth of paren/bracket/brace nesting
  bool backslash;    // in ST_{STRING,CHAR}, just seen backslash?
  bool star;         // in ST_C_COMMENT, just seen '*'?

  // so test code can interrogate internal state
  friend class CCSubstrateTest;

public:
  CCSubstrate(ReportError *err = NULL);
  virtual ~CCSubstrate();

  // EmbeddedLang entry points (see gramlex.h for description
  // of each function)
  virtual void reset(int initNest = 0);
  virtual void handle(char const *str, int len, char finalDelim);
  virtual bool zeroNesting() const;
  virtual sm_string getFuncBody() const;
  virtual sm_string getDeclName() const;
};

#endif // CCSSTR_H
