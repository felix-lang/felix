// useract.cc            see license.txt for copyright and terms of use
// code for useract.h

#include "elk_useract.h"
#include "sm_typ.h"
#include "sm_xassert.h"


UserActions::~UserActions()
{}


ParseTables *UserActions::makeTables()
{
  xfailure("this object does not have any tables");
  return NULL;   // silence warning
}


// ----------------- TrivialUserActions --------------------
UserActions::ReductionActionFunc TrivialUserActions::getReductionAction()
{
  return &TrivialUserActions::doReductionAction;
}

STATICDEF SemanticValue TrivialUserActions::doReductionAction(
  UserActions *, int , SemanticValue const *
  SOURCELOCARG( SourceLoc ) )
  { return NULL_SVAL; }

SemanticValue TrivialUserActions::duplicateTerminalValue(
  int , SemanticValue sval)
  { return sval; }

SemanticValue TrivialUserActions::duplicateNontermValue(
  int , SemanticValue sval)
  { return sval; }


void TrivialUserActions::deallocateTerminalValue(
  int , SemanticValue )
  {}

void TrivialUserActions::deallocateNontermValue(
  int , SemanticValue )
  {}

SemanticValue TrivialUserActions::mergeAlternativeParses(
  int , SemanticValue left, SemanticValue
  SOURCELOCARG( SourceLoc ) )
  { return left; }

bool TrivialUserActions::keepNontermValue(int , SemanticValue )
  { return true; }     // do not cancel


UserActions::ReclassifyFunc TrivialUserActions::getReclassifier()
{
  return &TrivialUserActions::reclassifyToken;
}

STATICDEF int TrivialUserActions::reclassifyToken(UserActions *,
  int oldTokenType, SemanticValue )
  { return oldTokenType; }

sm_string TrivialUserActions::terminalDescription(int, SemanticValue)
  { return sm_string(""); }

sm_string TrivialUserActions::nonterminalDescription(int, SemanticValue)
  { return sm_string(""); }

char const *TrivialUserActions::terminalName(int)
  { return ""; }
char const *TrivialUserActions::nonterminalName(int)
  { return ""; }

