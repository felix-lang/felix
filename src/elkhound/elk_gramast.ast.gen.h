// gramast.ast.gen.h
// *** DO NOT EDIT ***
// generated automatically by astgen, from gramast.ast

#ifndef GRAMAST_AST_GEN_H
#define GRAMAST_AST_GEN_H

#include "ast_asthelp.h"

// fwd decls
class GrammarAST;
class TopForm;
class TF_context;
class TF_verbatim;
class TF_option;
class TF_terminals;
class TF_nonterm;
class TermDecl;
class TermType;
class PrecSpec;
class SpecFunc;
class ProdDecl;
class RHSElt;
class RH_name;
class RH_sm_string;
class RH_prec;


// *** DO NOT EDIT ***

#include "ast_locstr.h"
#include "elk_asockind.h"

// *** DO NOT EDIT ***
class GrammarAST {
public:      // data
  ASTList <TopForm > forms;

public:      // funcs
  GrammarAST(ASTList <TopForm > *_forms) : forms(_forms) {
     { terms=NULL; firstNT=NULL; };
  }
  ~GrammarAST();

  char const *kindName() const { return "GrammarAST"; }

  GrammarAST *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  public:  TF_terminals *terms;
  public:  TF_nonterm *firstNT;
};



// *** DO NOT EDIT ***
class TopForm {
public:      // data

public:      // funcs
  TopForm() {
  }
  virtual ~TopForm();

  enum Kind { TF_CONTEXT, TF_VERBATIM, TF_OPTION, TF_TERMINALS, TF_NONTERM, NUM_KINDS };
  virtual Kind kind() const = 0;

  static char const * const kindNames[NUM_KINDS];
  char const *kindName() const { return kindNames[kind()]; }

  DECL_AST_DOWNCASTS(TF_context, TF_CONTEXT)
  DECL_AST_DOWNCASTS(TF_verbatim, TF_VERBATIM)
  DECL_AST_DOWNCASTS(TF_option, TF_OPTION)
  DECL_AST_DOWNCASTS(TF_terminals, TF_TERMINALS)
  DECL_AST_DOWNCASTS(TF_nonterm, TF_NONTERM)

  virtual TopForm *clone() const=0;

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};

class TF_context : public TopForm {
public:      // data
  LocString body;

public:      // funcs
  TF_context(LocString *_body) : TopForm(), body(_body) {
  }
  virtual ~TF_context();

  virtual Kind kind() const { return TF_CONTEXT; }
  enum { TYPE_TAG = TF_CONTEXT };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual TF_context *clone() const;

};

class TF_verbatim : public TopForm {
public:      // data
  bool isImpl;
  LocString code;

public:      // funcs
  TF_verbatim(bool _isImpl, LocString *_code) : TopForm(), isImpl(_isImpl), code(_code) {
  }
  virtual ~TF_verbatim();

  virtual Kind kind() const { return TF_VERBATIM; }
  enum { TYPE_TAG = TF_VERBATIM };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual TF_verbatim *clone() const;

};

class TF_option : public TopForm {
public:      // data
  LocString name;
  int value;

public:      // funcs
  TF_option(LocString *_name, int _value) : TopForm(), name(_name), value(_value) {
  }
  virtual ~TF_option();

  virtual Kind kind() const { return TF_OPTION; }
  enum { TYPE_TAG = TF_OPTION };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual TF_option *clone() const;

};

class TF_terminals : public TopForm {
public:      // data
  ASTList <TermDecl > decls;
  ASTList <TermType > types;
  ASTList <PrecSpec > prec;

public:      // funcs
  TF_terminals(ASTList <TermDecl > *_decls, ASTList <TermType > *_types, ASTList <PrecSpec > *_prec) : TopForm(), decls(_decls), types(_types), prec(_prec) {
  }
  virtual ~TF_terminals();

  virtual Kind kind() const { return TF_TERMINALS; }
  enum { TYPE_TAG = TF_TERMINALS };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual TF_terminals *clone() const;

};

class TF_nonterm : public TopForm {
public:      // data
  LocString name;
  LocString type;
  ASTList <SpecFunc > funcs;
  ASTList <ProdDecl > productions;
  ASTList <LocString > subsets;

public:      // funcs
  TF_nonterm(LocString *_name, LocString *_type, ASTList <SpecFunc > *_funcs, ASTList <ProdDecl > *_productions, ASTList <LocString > *_subsets) : TopForm(), name(_name), type(_type), funcs(_funcs), productions(_productions), subsets(_subsets) {
  }
  virtual ~TF_nonterm();

  virtual Kind kind() const { return TF_NONTERM; }
  enum { TYPE_TAG = TF_NONTERM };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual TF_nonterm *clone() const;

};



// *** DO NOT EDIT ***
class TermDecl {
public:      // data
  int code;
  LocString name;
  LocString alias;

public:      // funcs
  TermDecl(int _code, LocString *_name, LocString *_alias) : code(_code), name(_name), alias(_alias) {
  }
  ~TermDecl();

  char const *kindName() const { return "TermDecl"; }

  TermDecl *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};



// *** DO NOT EDIT ***
class TermType {
public:      // data
  LocString name;
  LocString type;
  ASTList <SpecFunc > funcs;

public:      // funcs
  TermType(LocString *_name, LocString *_type, ASTList <SpecFunc > *_funcs) : name(_name), type(_type), funcs(_funcs) {
  }
  ~TermType();

  char const *kindName() const { return "TermType"; }

  TermType *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};



// *** DO NOT EDIT ***
class PrecSpec {
public:      // data
  AssocKind kind;
  int prec;
  ASTList <LocString > tokens;

public:      // funcs
  PrecSpec(AssocKind _kind, int _prec, ASTList <LocString > *_tokens) : kind(_kind), prec(_prec), tokens(_tokens) {
  }
  ~PrecSpec();

  char const *kindName() const { return "PrecSpec"; }

  PrecSpec *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};



// *** DO NOT EDIT ***
class SpecFunc {
public:      // data
  LocString name;
  ASTList <LocString > formals;
  LocString code;

public:      // funcs
  SpecFunc(LocString *_name, ASTList <LocString > *_formals, LocString *_code) : name(_name), formals(_formals), code(_code) {
  }
  ~SpecFunc();

  char const *kindName() const { return "SpecFunc"; }

  SpecFunc *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  public:  LocString nthFormal(int i) const
    { return *( formals.nthC(i) ); };
};



// *** DO NOT EDIT ***
class ProdDecl {
public:      // data
  ASTList <RHSElt > rhs;
  LocString actionCode;

public:      // funcs
  ProdDecl(ASTList <RHSElt > *_rhs, LocString *_actionCode) : rhs(_rhs), actionCode(_actionCode) {
  }
  ~ProdDecl();

  char const *kindName() const { return "ProdDecl"; }

  ProdDecl *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};



// *** DO NOT EDIT ***
class RHSElt {
public:      // data

public:      // funcs
  RHSElt() {
  }
  virtual ~RHSElt();

  enum Kind { RH_NAME, RH_STRING, RH_PREC, NUM_KINDS };
  virtual Kind kind() const = 0;

  static char const * const kindNames[NUM_KINDS];
  char const *kindName() const { return kindNames[kind()]; }

  DECL_AST_DOWNCASTS(RH_name, RH_NAME)
  DECL_AST_DOWNCASTS(RH_sm_string, RH_STRING)
  DECL_AST_DOWNCASTS(RH_prec, RH_PREC)

  virtual RHSElt *clone() const=0;

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};

class RH_name : public RHSElt {
public:      // data
  LocString tag;
  LocString name;

public:      // funcs
  RH_name(LocString *_tag, LocString *_name) : RHSElt(), tag(_tag), name(_name) {
  }
  virtual ~RH_name();

  virtual Kind kind() const { return RH_NAME; }
  enum { TYPE_TAG = RH_NAME };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual RH_name *clone() const;

};

class RH_sm_string : public RHSElt {
public:      // data
  LocString tag;
  LocString str;

public:      // funcs
  RH_sm_string(LocString *_tag, LocString *_str) : RHSElt(), tag(_tag), str(_str) {
  }
  virtual ~RH_sm_string();

  virtual Kind kind() const { return RH_STRING; }
  enum { TYPE_TAG = RH_STRING };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual RH_sm_string *clone() const;

};

class RH_prec : public RHSElt {
public:      // data
  LocString tokName;

public:      // funcs
  RH_prec(LocString *_tokName) : RHSElt(), tokName(_tokName) {
  }
  virtual ~RH_prec();

  virtual Kind kind() const { return RH_PREC; }
  enum { TYPE_TAG = RH_PREC };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual RH_prec *clone() const;

};



#endif // GRAMAST_AST_GEN_H
