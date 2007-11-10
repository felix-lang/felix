// ast.ast.h
// *** DO NOT EDIT ***
// generated automatically by astgen, from ast.ast

#ifndef AST_AST_H
#define AST_AST_H

#include "ast_asthelp.h"

// fwd decls
class ASTSpecFile;
class ToplevelForm;
class TF_verbatim;
class TF_impl_verbatim;
class TF_class;
class TF_option;
class TF_enum;
class ASTClass;
class AccessMod;
class Annotation;
class UserDecl;
class CustomCode;
class CtorArg;
class BaseClass;


// *** DO NOT EDIT ***

#include "sm_str.h"

  // this signals to ast.hand.cc that ast.ast.cc is nonempty,
  // so none of the bootstrap code in ast.hand.cc should be used
  #define GENERATED_AST_PRESENT

// *** DO NOT EDIT ***
class ASTSpecFile {
public:      // data
  ASTList <ToplevelForm > forms;

public:      // funcs
  ASTSpecFile(ASTList <ToplevelForm > *_forms) : forms(_forms) {
  }
  ~ASTSpecFile();

  char const *kindName() const { return "ASTSpecFile"; }

  ASTSpecFile *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};



// *** DO NOT EDIT ***
class ToplevelForm {
public:      // data

public:      // funcs
  ToplevelForm() {
  }
  virtual ~ToplevelForm();

  enum Kind { TF_VERBATIM, TF_IMPL_VERBATIM, TF_CLASS, TF_OPTION, TF_ENUM, NUM_KINDS };
  virtual Kind kind() const = 0;

  static char const * const kindNames[NUM_KINDS];
  char const *kindName() const { return kindNames[kind()]; }

  DECL_AST_DOWNCASTS(TF_verbatim, TF_VERBATIM)
  DECL_AST_DOWNCASTS(TF_impl_verbatim, TF_IMPL_VERBATIM)
  DECL_AST_DOWNCASTS(TF_class, TF_CLASS)
  DECL_AST_DOWNCASTS(TF_option, TF_OPTION)
  DECL_AST_DOWNCASTS(TF_enum, TF_ENUM)

  virtual ToplevelForm *clone() const=0;

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};

class TF_verbatim : public ToplevelForm {
public:      // data
  sm_string code;

public:      // funcs
  TF_verbatim(sm_string _code) : ToplevelForm(), code(_code) {
  }
  virtual ~TF_verbatim();

  virtual Kind kind() const { return TF_VERBATIM; }
  enum { TYPE_TAG = TF_VERBATIM };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual TF_verbatim *clone() const;

};

class TF_impl_verbatim : public ToplevelForm {
public:      // data
  sm_string code;

public:      // funcs
  TF_impl_verbatim(sm_string _code) : ToplevelForm(), code(_code) {
  }
  virtual ~TF_impl_verbatim();

  virtual Kind kind() const { return TF_IMPL_VERBATIM; }
  enum { TYPE_TAG = TF_IMPL_VERBATIM };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual TF_impl_verbatim *clone() const;

};

class TF_class : public ToplevelForm {
public:      // data
  ASTClass *super;
  ASTList <ASTClass > ctors;

public:      // funcs
  TF_class(ASTClass *_super, ASTList <ASTClass > *_ctors) : ToplevelForm(), super(_super), ctors(_ctors) {
  }
  virtual ~TF_class();

  virtual Kind kind() const { return TF_CLASS; }
  enum { TYPE_TAG = TF_CLASS };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual TF_class *clone() const;

  public:  bool hasChildren() const { return ctors.isNotEmpty(); };
};

class TF_option : public ToplevelForm {
public:      // data
  sm_string name;
  ASTList <sm_string > args;

public:      // funcs
  TF_option(sm_string _name, ASTList <sm_string > *_args) : ToplevelForm(), name(_name), args(_args) {
  }
  virtual ~TF_option();

  virtual Kind kind() const { return TF_OPTION; }
  enum { TYPE_TAG = TF_OPTION };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual TF_option *clone() const;

};

class TF_enum : public ToplevelForm {
public:      // data
  sm_string name;
  ASTList <sm_string > enumerators;

public:      // funcs
  TF_enum(sm_string _name, ASTList <sm_string > *_enumerators) : ToplevelForm(), name(_name), enumerators(_enumerators) {
  }
  virtual ~TF_enum();

  virtual Kind kind() const { return TF_ENUM; }
  enum { TYPE_TAG = TF_ENUM };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual TF_enum *clone() const;

};



// *** DO NOT EDIT ***
class ASTClass {
public:      // data
  sm_string name;
  ASTList <CtorArg > args;
  ASTList <BaseClass > bases;
  ASTList <Annotation > decls;

public:      // funcs
  ASTClass(sm_string _name, ASTList <CtorArg > *_args, ASTList <BaseClass > *_bases, ASTList <Annotation > *_decls) : name(_name), args(_args), bases(_bases), decls(_decls) {
  }
  ~ASTClass();

  char const *kindName() const { return "ASTClass"; }

  ASTClass *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  public:  sm_string classKindName() const;
};



// *** DO NOT EDIT ***

  // specifies what kind of userdecl this is; pub/priv/prot are uninterpreted
  // class members with the associated access control; ctor and dtor are
  // code to be inserted into the ctor or dtor, respectively
  enum AccessCtl {
    AC_PUBLIC,      // access
    AC_PRIVATE,     //   control
    AC_PROTECTED,   //     keywords
    AC_CTOR,        // insert into ctor
    AC_DTOR,        // insert into dtor
    AC_PUREVIRT,    // declare pure virtual in superclass, and impl in subclass
    NUM_ACCESSCTLS
  };

  // map the enum value to a sm_string like "public"
  sm_string toString(AccessCtl acc);      // defined in ast.cc

// *** DO NOT EDIT ***
class AccessMod {
public:      // data
  AccessCtl acc;
  ASTList <sm_string > mods;

public:      // funcs
  AccessMod(AccessCtl _acc, ASTList <sm_string > *_mods) : acc(_acc), mods(_mods) {
  }
  ~AccessMod();

  char const *kindName() const { return "AccessMod"; }

  AccessMod *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  public:  bool hasMod(char const *mod) const;
};



// *** DO NOT EDIT ***
class Annotation {
public:      // data

public:      // funcs
  Annotation() {
  }
  virtual ~Annotation();

  enum Kind { USERDECL, CUSTOMCODE, NUM_KINDS };
  virtual Kind kind() const = 0;

  static char const * const kindNames[NUM_KINDS];
  char const *kindName() const { return kindNames[kind()]; }

  DECL_AST_DOWNCASTS(UserDecl, USERDECL)
  DECL_AST_DOWNCASTS(CustomCode, CUSTOMCODE)

  virtual Annotation *clone() const=0;

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};

class UserDecl : public Annotation {
public:      // data
  AccessMod *amod;
  sm_string code;
  sm_string init;

public:      // funcs
  UserDecl(AccessMod *_amod, sm_string _code, sm_string _init) : Annotation(), amod(_amod), code(_code), init(_init) {
  }
  virtual ~UserDecl();

  virtual Kind kind() const { return USERDECL; }
  enum { TYPE_TAG = USERDECL };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual UserDecl *clone() const;

  public:  AccessCtl access() const { return amod->acc; };
};

class CustomCode : public Annotation {
public:      // data
  sm_string qualifier;
  sm_string code;

public:      // funcs
  CustomCode(sm_string _qualifier, sm_string _code) : Annotation(), qualifier(_qualifier), code(_code) {
     used=false;
  }
  virtual ~CustomCode();

  virtual Kind kind() const { return CUSTOMCODE; }
  enum { TYPE_TAG = CUSTOMCODE };

  virtual void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

  virtual CustomCode *clone() const;

  public:  bool used;
};



// *** DO NOT EDIT ***
class CtorArg {
public:      // data
  bool isOwner;
  sm_string type;
  sm_string name;
  sm_string defaultValue;

public:      // funcs
  CtorArg(bool _isOwner, sm_string _type, sm_string _name, sm_string _defaultValue) : isOwner(_isOwner), type(_type), name(_name), defaultValue(_defaultValue) {
  }
  ~CtorArg();

  char const *kindName() const { return "CtorArg"; }

  CtorArg *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};



// *** DO NOT EDIT ***
class BaseClass {
public:      // data
  AccessCtl access;
  sm_string name;

public:      // funcs
  BaseClass(AccessCtl _access, sm_string _name) : access(_access), name(_name) {
  }
  ~BaseClass();

  char const *kindName() const { return "BaseClass"; }

  BaseClass *clone() const;

  void debugPrint(std::ostream &os, int indent, char const *subtreeName = "tree") const;

};




#endif // AST_AST_H
