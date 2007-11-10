// mlsstr.h            see license.txt for copyright and terms of use
// handles lexically embedded ML
// based on ccsstr.h

#ifndef MLSSTR_H
#define MLSSTR_H

#include "ast_embedded.h"

class MLSubstrateTest;

class MLSubstrate : public EmbeddedLang {
private:
  enum State {
    ST_NORMAL,       // normal text
    ST_STRING,       // inside a sm_string literal
    ST_CHAR,         // inside a char literal
    ST_COMMENT,      // inside a comment
    NUM_STATES
  } state;
  int nesting;       // depth of paren/bracket/brace nesting
  int comNesting;    // depth of comment nesting (in ST_COMMENT)
  char prev;         // previous character

  // so test code can interrogate internal state
  friend class MLSubstrateTest;

public:
  MLSubstrate(ReportError *err = NULL);
  virtual ~MLSubstrate();

  // EmbeddedLang entry points (see gramlex.h for description
  // of each function)
  virtual void reset(int initNest = 0);
  virtual void handle(char const *str, int len, char finalDelim);
  virtual bool zeroNesting() const;
  virtual sm_string getFuncBody() const;
  virtual sm_string getDeclName() const;
};

#endif // MLSSTR_H
