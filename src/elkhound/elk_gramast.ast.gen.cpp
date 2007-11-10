// gramast.ast.gen.cc
// *** DO NOT EDIT ***
// generated automatically by astgen, from gramast.ast

#include "elk_gramast.ast.gen.h"


// ------------------ GrammarAST -------------------
// *** DO NOT EDIT ***
GrammarAST::~GrammarAST()
{
  forms.deleteAll();
}

void GrammarAST::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, GrammarAST);

  PRINT_LIST(TopForm, forms);
}

GrammarAST *GrammarAST::clone() const
{
  GrammarAST *ret = new GrammarAST(
    cloneASTList(forms)
  );
  return ret;
}


// ------------------ TopForm -------------------
// *** DO NOT EDIT ***
TopForm::~TopForm()
{
}

char const * const TopForm::kindNames[TopForm::NUM_KINDS] = {
  "TF_context",
  "TF_verbatim",
  "TF_option",
  "TF_terminals",
  "TF_nonterm",
};

void TopForm::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
}

DEFN_AST_DOWNCASTS(TopForm, TF_context, TF_CONTEXT)

TF_context::~TF_context()
{
}

void TF_context::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TF_context);

  TopForm::debugPrint(os, indent, subtreeName);

  PRINT_GENERIC(body);
}

TF_context *TF_context::clone() const
{
  TF_context *ret = new TF_context(
    body.clone()
  );
  return ret;
}

DEFN_AST_DOWNCASTS(TopForm, TF_verbatim, TF_VERBATIM)

TF_verbatim::~TF_verbatim()
{
}

void TF_verbatim::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TF_verbatim);

  TopForm::debugPrint(os, indent, subtreeName);

  PRINT_BOOL(isImpl);
  PRINT_GENERIC(code);
}

TF_verbatim *TF_verbatim::clone() const
{
  TF_verbatim *ret = new TF_verbatim(
    isImpl,
    code.clone()
  );
  return ret;
}

DEFN_AST_DOWNCASTS(TopForm, TF_option, TF_OPTION)

TF_option::~TF_option()
{
}

void TF_option::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TF_option);

  TopForm::debugPrint(os, indent, subtreeName);

  PRINT_GENERIC(name);
  PRINT_GENERIC(value);
}

TF_option *TF_option::clone() const
{
  TF_option *ret = new TF_option(
    name.clone(),
    value
  );
  return ret;
}

DEFN_AST_DOWNCASTS(TopForm, TF_terminals, TF_TERMINALS)

TF_terminals::~TF_terminals()
{
  decls.deleteAll();
  types.deleteAll();
  prec.deleteAll();
}

void TF_terminals::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TF_terminals);

  TopForm::debugPrint(os, indent, subtreeName);

  PRINT_LIST(TermDecl, decls);
  PRINT_LIST(TermType, types);
  PRINT_LIST(PrecSpec, prec);
}

TF_terminals *TF_terminals::clone() const
{
  TF_terminals *ret = new TF_terminals(
    cloneASTList(decls),
    cloneASTList(types),
    cloneASTList(prec)
  );
  return ret;
}

DEFN_AST_DOWNCASTS(TopForm, TF_nonterm, TF_NONTERM)

TF_nonterm::~TF_nonterm()
{
  funcs.deleteAll();
  productions.deleteAll();
  subsets.deleteAll();
}

void TF_nonterm::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TF_nonterm);

  TopForm::debugPrint(os, indent, subtreeName);

  PRINT_GENERIC(name);
  PRINT_GENERIC(type);
  PRINT_LIST(SpecFunc, funcs);
  PRINT_LIST(ProdDecl, productions);
  PRINT_LIST(LocString, subsets);
}

TF_nonterm *TF_nonterm::clone() const
{
  TF_nonterm *ret = new TF_nonterm(
    name.clone(),
    type.clone(),
    cloneASTList(funcs),
    cloneASTList(productions),
    cloneASTList(subsets)
  );
  return ret;
}


// ------------------ TermDecl -------------------
// *** DO NOT EDIT ***
TermDecl::~TermDecl()
{
}

void TermDecl::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TermDecl);

  PRINT_GENERIC(code);
  PRINT_GENERIC(name);
  PRINT_GENERIC(alias);
}

TermDecl *TermDecl::clone() const
{
  TermDecl *ret = new TermDecl(
    code,
    name.clone(),
    alias.clone()
  );
  return ret;
}


// ------------------ TermType -------------------
// *** DO NOT EDIT ***
TermType::~TermType()
{
  funcs.deleteAll();
}

void TermType::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TermType);

  PRINT_GENERIC(name);
  PRINT_GENERIC(type);
  PRINT_LIST(SpecFunc, funcs);
}

TermType *TermType::clone() const
{
  TermType *ret = new TermType(
    name.clone(),
    type.clone(),
    cloneASTList(funcs)
  );
  return ret;
}


// ------------------ PrecSpec -------------------
// *** DO NOT EDIT ***
PrecSpec::~PrecSpec()
{
  tokens.deleteAll();
}

void PrecSpec::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, PrecSpec);

  PRINT_GENERIC(kind);
  PRINT_GENERIC(prec);
  PRINT_LIST(LocString, tokens);
}

PrecSpec *PrecSpec::clone() const
{
  PrecSpec *ret = new PrecSpec(
    kind,
    prec,
    cloneASTList(tokens)
  );
  return ret;
}


// ------------------ SpecFunc -------------------
// *** DO NOT EDIT ***
SpecFunc::~SpecFunc()
{
  formals.deleteAll();
}

void SpecFunc::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, SpecFunc);

  PRINT_GENERIC(name);
  PRINT_LIST(LocString, formals);
  PRINT_GENERIC(code);
}

SpecFunc *SpecFunc::clone() const
{
  SpecFunc *ret = new SpecFunc(
    name.clone(),
    cloneASTList(formals),
    code.clone()
  );
  return ret;
}


// ------------------ ProdDecl -------------------
// *** DO NOT EDIT ***
ProdDecl::~ProdDecl()
{
  rhs.deleteAll();
}

void ProdDecl::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, ProdDecl);

  PRINT_LIST(RHSElt, rhs);
  PRINT_GENERIC(actionCode);
}

ProdDecl *ProdDecl::clone() const
{
  ProdDecl *ret = new ProdDecl(
    cloneASTList(rhs),
    actionCode.clone()
  );
  return ret;
}


// ------------------ RHSElt -------------------
// *** DO NOT EDIT ***
RHSElt::~RHSElt()
{
}

char const * const RHSElt::kindNames[RHSElt::NUM_KINDS] = {
  "RH_name",
  "RH_sm_string",
  "RH_prec",
};

void RHSElt::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
}

DEFN_AST_DOWNCASTS(RHSElt, RH_name, RH_NAME)

RH_name::~RH_name()
{
}

void RH_name::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, RH_name);

  RHSElt::debugPrint(os, indent, subtreeName);

  PRINT_GENERIC(tag);
  PRINT_GENERIC(name);
}

RH_name *RH_name::clone() const
{
  RH_name *ret = new RH_name(
    tag.clone(),
    name.clone()
  );
  return ret;
}

DEFN_AST_DOWNCASTS(RHSElt, RH_sm_string, RH_STRING)

RH_sm_string::~RH_sm_string()
{
}

void RH_sm_string::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, RH_sm_string);

  RHSElt::debugPrint(os, indent, subtreeName);

  PRINT_GENERIC(tag);
  PRINT_GENERIC(str);
}

RH_sm_string *RH_sm_string::clone() const
{
  RH_sm_string *ret = new RH_sm_string(
    tag.clone(),
    str.clone()
  );
  return ret;
}

DEFN_AST_DOWNCASTS(RHSElt, RH_prec, RH_PREC)

RH_prec::~RH_prec()
{
}

void RH_prec::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, RH_prec);

  RHSElt::debugPrint(os, indent, subtreeName);

  PRINT_GENERIC(tokName);
}

RH_prec *RH_prec::clone() const
{
  RH_prec *ret = new RH_prec(
    tokName.clone()
  );
  return ret;
}




