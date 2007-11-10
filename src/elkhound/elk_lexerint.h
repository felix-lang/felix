// lexerint.h            see license.txt for copyright and terms of use
// LexerInterface, the interface the GLR parser uses
// to access the lexer's token stream

#ifndef LEXERINT_H
#define LEXERINT_H

#include "elk_useract.h"
#include "sm_srcloc.h"
#include "sm_str.h"

// This 'interface' is a collection of variables describing
// the current token.  I don't use a bunch of pure-virtual
// functions because of the cost of calling them; everything
// here will be in the inner loop of the parser.
class LexerInterface {
public:     // data
  // NOTE: All of these fields are *written* by the lexer, and
  // *read* by the parser.

  // token classification; this is what the parser will use to
  // make parsing decisions; this code must correspond to something
  // declared in the 'terminals' section of the grammar; when this
  // is 0, it is the final (end-of-file) token; the parser is allowed
  // to change this for its own purposes, and currently does so for
  // token reclassification
  int type;

  // semantic value; this is what will be passed to the reduction
  // actions when this token is on the right hand side of a rule
  SemanticValue sval;

  // source location of the token; this will only be used if the
  // parser has been compiled to automatically propagate it
  SourceLoc loc;

public:     // funcs
  LexerInterface()
    : type(0),
      sval(0),
      loc(SL_UNKNOWN)
  {}
  virtual ~LexerInterface() {}


  // retrieve the next token; the lexer should respond by filling in
  // the above fields with new values, to describe the next token; the
  // lexer indicates end of file by putting 0 into 'type'; when the
  // LexerInterface object is first passed to the parser, the above
  // fields should already be set correctly (i.e. the parser will make
  // its first call to 'nextToken' *after* processing the first token)
  typedef void (*NextTokenFunc)(LexerInterface *);

  // get the function which we'll call to get the next token
  //
  // Why the two-step approach?  Virtual method calls are more
  // expensive than simple indirect function calls, and this happens
  // in the inner parsing loop.  If C++ had a way to explicitly cache
  // the result of a method lookup this wouldn't be necessary.
  virtual NextTokenFunc getTokenFunc() const=0;


  // The following functions are called to help create diagnostic
  // reports.  They should describe the current token (the one
  // which the above fields refer to) in more-or-less human-readable
  // terms.

  // describe the token; for tokens with multiple spellings (e.g.
  // identifiers), this should include the actual token spelling
  // if possible; note that if the token has been reclassified,
  // then the 'type' field above might have been changed by the
  // parser, in which case this function should ideally print
  // a description which takes the new type into account
  virtual sm_string tokenDesc() const=0;

  // describe a token kind; this is different from tokenDesc(), since
  // it need not correspond to the token kind that was just yielded,
  // and hence any related lexeme data cannot be assumed to be
  // available; this is used during error diagnosis
  virtual sm_string tokenKindDesc(int kind) const=0;
};

#endif // LEXERINT_H
