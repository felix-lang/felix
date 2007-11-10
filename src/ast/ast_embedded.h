// embedded.h            see license.txt for copyright and terms of use
// interface to an embedded language processor

#ifndef EMBEDDED_H
#define EMBEDDED_H

#include "sm_str.h"
#include "ast_reporterr.h"

class EmbeddedLang {
public:
  // for reporting errors
  ReportError *err;

  // all text processed so far; it collects the
  // embedded code; clients will call 'handle' a
  // bunch of times and then expect to retrieve
  // the text from here
  sm_stringBuilder text;

  // when true (set by the lexer), the 'text' is to
  // be interpreted as an expression, rather than a
  // complete function body; this affects what
  // getFuncBody() returns
  bool exprOnly;

  // when true the text is a declaration, so we have to
  // add a single semicolon
  bool isDeclaration;

public:
  EmbeddedLang(ReportError *err = NULL /*print to stdout*/);
  virtual ~EmbeddedLang();    // silence warning

  // start from scratch
  virtual void reset(int initNest = 0) = 0;

  // process the given sm_string of characters, as source text in
  // the embedded language; 'finalDelim' is provided for printing
  // informative error messages
  virtual void handle(char const *str, int len, char finalDelim) = 0;

  // return true if we're at a nesting level of zero
  // and not in a sm_string, etc. -- characters at this
  // level have "usual" meaning
  virtual bool zeroNesting() const = 0;

  // return the body of the embedded function; should
  // always return a complete function body, even when
  // exprOnly is true (by adding to 'text' if necessary)
  virtual sm_string getFuncBody() const = 0;

  // return the name of the declared function, assuming
  // that is the context in which 'text' was collected
  virtual sm_string getDeclName() const = 0;
};

#endif // EMBEDDED_H
