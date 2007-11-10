// ast.hand.h            see license.txt for copyright and terms of use
// generated (by hand) from ast.ast

#ifndef BOOTSTRAP
  // in non-bootstrap mode, use the generated file
#include "ast_ast.ast.h"
#endif

#ifndef GENERATED_AST_PRESENT
// use the code below if either:
//   - BOOTSTRAP was defined
//   - ast.gen.h was empty

#ifndef AST_HAND_H
#define AST_HAND_H

#include "ast_asthelp.h"

// fwd decls
class ASTSpecFile;
class ToplevelForm;
class TF_verbatim;
class ASTClass;
class UserDecl;
class ASTCtor;
class CtorArg;


#include "sm_str.h"

class ASTSpecFile {
public:
  ASTList<ToplevelForm> forms;

public:
  ASTSpecFile(ASTList<ToplevelForm> *_forms) : forms(_forms) {}
  ~ASTSpecFile() {}

  void debugPrint(std::ostream &os, int indent) const;
};


class ToplevelForm {
public:
  ToplevelForm() {}
  virtual ~ToplevelForm() {}

  enum Kind { TF_VERBATIM, ASTCLASS, NUM_KINDS };
  virtual Kind kind() const = 0;

  DECL_AST_DOWNCASTS(TF_verbatim, TF_VERBATIM)
  DECL_AST_DOWNCASTS(ASTClass, ASTCLASS)

  virtual void debugPrint(std::ostream &os, int indent) const;
};

class TF_verbatim : public ToplevelForm {
public:
  sm_string code;

public:
  TF_verbatim(sm_string _code)
    : code(_code)
  {}
  virtual ~TF_verbatim() {}

  virtual Kind kind() const { return TF_VERBATIM; }
  enum { TYPE_TAG = TF_VERBATIM };

  virtual void debugPrint(std::ostream &os, int indent) const;
};

class ASTClass : public ToplevelForm {
public:
  sm_string name;
  ASTList<CtorArg> superCtor;
  ASTList<UserDecl> decls;
  ASTList<ASTCtor> ctors;

public:
  ASTClass(sm_string _name, ASTList<CtorArg> *_superCtor,
           ASTList<UserDecl> *_decls, ASTList<ASTCtor> *_ctors) :
    name(_name), superCtor(_superCtor), decls(_decls), ctors(_ctors)
  {}
  ~ASTClass() {}

  virtual Kind kind() const { return ASTCLASS; }
  enum { TYPE_TAG = ASTCLASS };

  virtual void debugPrint(std::ostream &os, int indent) const;

  public: bool hasChildren() const { return ctors.isNotEmpty(); }
};


  enum AccessCtl { AC_PUBLIC, AC_PRIVATE, AC_PROTECTED };
  sm_string toString(AccessCtl acc);      // defined in ast.cc


class UserDecl {
public:
  AccessCtl access;
  sm_string code;

public:
  UserDecl(AccessCtl _access, sm_string _code)
    : access(_access), code(_code)
  {}
  ~UserDecl() {}

  void debugPrint(std::ostream &os, int indent) const;
};


class ASTCtor {
public:
  sm_string name;
  ASTList<CtorArg> args;
  ASTList<UserDecl> decls;

public:
  ASTCtor(sm_string _name, ASTList<CtorArg> *_args, ASTList<UserDecl> *_decls)
    : name(_name), args(_args), decls(_decls)
  {}
  ~ASTCtor() {}

  void debugPrint(std::ostream &os, int indent) const;

  public: sm_string kindName() const;
};


class CtorArg {
public:
  bool owner;
  sm_string type;
  sm_string name;

public:
  CtorArg(bool _owner, sm_string _type, sm_string _name)
    : owner(_owner),
      type(_type),
      name(_name)
  {}
  ~CtorArg() {}

  void debugPrint(std::ostream &os, int indent) const;
};

#endif // AST_HAND_H

#endif // !GENERATED_AST_PRESENT
