// ptreeact.h            see license.txt for copyright and terms of use
// a generic set of user actions that build parse trees for any grammar

#ifndef PTREEACT_H
#define PTREEACT_H

#include "elk_lexerint.h"
#include "elk_useract.h"

class ParseTables;         // parsetables.h


// lexer to yield PTreeNodes for tokens
class ParseTreeLexer : public LexerInterface {
private:
  LexerInterface *underlying;   // for getting token descriptions
  NextTokenFunc underToken;     // for getting tokens
  UserActions *actions;         // for getting symbol names

private:
  void copyFields();

public:
  ParseTreeLexer(LexerInterface *u, UserActions *a);

  static void nextToken(LexerInterface *lex);
  virtual NextTokenFunc getTokenFunc() const
    { return &ParseTreeLexer::nextToken; }

  virtual sm_string tokenDesc() const;
  virtual sm_string tokenKindDesc(int kind) const;
};


// layer these actions on top of the generated actions to
// build parse trees for the reductions
class ParseTreeActions : public TrivialUserActions {
private:
  UserActions *underlying;   // for getting symbol names
  ParseTables *tables;       // for finding out production lengths

public:
  ParseTreeActions(UserActions *u, ParseTables *t)
    : underlying(u), tables(t) {}

  static SemanticValue reduce(
    UserActions *context,
    int productionId,
    SemanticValue const *svals
    SOURCELOCARG( SourceLoc loc ) );
  virtual ReductionActionFunc getReductionAction()
    { return &ParseTreeActions::reduce; }

  virtual SemanticValue mergeAlternativeParses(
    int ntIndex, SemanticValue left, SemanticValue right
    SOURCELOCARG( SourceLoc loc ) );

  virtual char const *terminalName(int termId);
  virtual char const *nonterminalName(int termId);

  ParseTables *getTables() { return tables; }
};


#endif // PTREEACT_H
