// ptreeact.cc            see license.txt for copyright and terms of use
// code for ptreeact.h

#include "elk_ptreeact.h"
#include "elk_ptreenode.h"
#include "elk_parsetables.h"
#include "sm_trace.h"


// ------------------- ParseTreeLexer -------------------
ParseTreeLexer::ParseTreeLexer(LexerInterface *u, UserActions *a)
  : underlying(u),
    underToken(u->getTokenFunc()),
    actions(a)
{
  // the underlying lexer is already primed
  copyFields();
}

STATICDEF void ParseTreeLexer::nextToken(LexerInterface *lex)
{
  ParseTreeLexer *ths = static_cast<ParseTreeLexer*>(lex);

  // call underlying token function
  ths->underToken(ths->underlying);

  // grab its fields
  ths->copyFields();
}

void ParseTreeLexer::copyFields()
{
  type = underlying->type;
  loc = underlying->loc;

  // leak underlying's 'sval'.. we'll just assume it doesn't matter

  // my sval is always a newly-allocated PTreeNode, with no children,
  // and named according to the name of the token yielded
  PTreeNode *ret = new PTreeNode(actions->terminalName(type));
  sval = (SemanticValue)ret;
}


sm_string ParseTreeLexer::tokenDesc() const
{
  return underlying->tokenDesc();
}

sm_string ParseTreeLexer::tokenKindDesc(int kind) const
{
  return underlying->tokenKindDesc(kind);
}


// ---------------------- ParseTreeActions -------------------
STATICDEF SemanticValue ParseTreeActions::reduce(
  UserActions *context,
  int productionId,
  SemanticValue const *svals
  SOURCELOCARG( SourceLoc loc ) )
{
  ParseTreeActions *ths = static_cast<ParseTreeActions*>(context);

  // get info about this production
  ParseTables::ProdInfo const &info = ths->tables->getProdInfo(productionId);
  xassert(info.rhsLen <= PTreeNode::MAXCHILDREN);

  // make a bare PTreeNode, labeled with the LHS nonterminal name
  PTreeNode *ret = new PTreeNode(ths->underlying->nonterminalName(info.lhsIndex));

  // add the children
  for (int i=0; i < info.rhsLen; i++) {
    ret->children[i] = (PTreeNode*)svals[i];
  }
  ret->numChildren = info.rhsLen;

  return (SemanticValue)ret;
}


SemanticValue ParseTreeActions::mergeAlternativeParses(
  int ntIndex, SemanticValue left, SemanticValue right
  SOURCELOCARG( SourceLoc loc ) )
{
  trace("ptreeactMerge") << underlying->nonterminalName(ntIndex) << "\n";

  // link the ambiguities together in the usual way
  PTreeNode *L = (PTreeNode*)left;
  PTreeNode *R = (PTreeNode*)right;

  L->addAlternative(R);
  return left;
}


char const *ParseTreeActions::terminalName(int termId)
{
  return underlying->terminalName(termId);
}

char const *ParseTreeActions::nonterminalName(int termId)
{
  return underlying->nonterminalName(termId);
}
