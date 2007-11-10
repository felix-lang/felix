// ast.ast.cc
// *** DO NOT EDIT ***
// generated automatically by astgen, from ast.ast

#include "ast_ast.ast.h"


// ------------------ ASTSpecFile -------------------
// *** DO NOT EDIT ***
ASTSpecFile::~ASTSpecFile()
{
  forms.deleteAll();
}

void ASTSpecFile::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, ASTSpecFile);

  PRINT_LIST(ToplevelForm, forms);
}

ASTSpecFile *ASTSpecFile::clone() const
{
  ASTSpecFile *ret = new ASTSpecFile(
    cloneASTList(forms)
  );
  return ret;
}


// ------------------ ToplevelForm -------------------
// *** DO NOT EDIT ***
ToplevelForm::~ToplevelForm()
{
}

char const * const ToplevelForm::kindNames[ToplevelForm::NUM_KINDS] = {
  "TF_verbatim",
  "TF_impl_verbatim",
  "TF_class",
  "TF_option",
  "TF_enum",
};

void ToplevelForm::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
}

DEFN_AST_DOWNCASTS(ToplevelForm, TF_verbatim, TF_VERBATIM)

TF_verbatim::~TF_verbatim()
{
}

void TF_verbatim::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TF_verbatim);

  ToplevelForm::debugPrint(os, indent, subtreeName);

  PRINT_STRING(code);
}

TF_verbatim *TF_verbatim::clone() const
{
  TF_verbatim *ret = new TF_verbatim(
    code
  );
  return ret;
}

DEFN_AST_DOWNCASTS(ToplevelForm, TF_impl_verbatim, TF_IMPL_VERBATIM)

TF_impl_verbatim::~TF_impl_verbatim()
{
}

void TF_impl_verbatim::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TF_impl_verbatim);

  ToplevelForm::debugPrint(os, indent, subtreeName);

  PRINT_STRING(code);
}

TF_impl_verbatim *TF_impl_verbatim::clone() const
{
  TF_impl_verbatim *ret = new TF_impl_verbatim(
    code
  );
  return ret;
}

DEFN_AST_DOWNCASTS(ToplevelForm, TF_class, TF_CLASS)

TF_class::~TF_class()
{
  delete super;
  ctors.deleteAll();
}

void TF_class::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TF_class);

  ToplevelForm::debugPrint(os, indent, subtreeName);

  PRINT_SUBTREE(super);
  PRINT_LIST(ASTClass, ctors);
}

TF_class *TF_class::clone() const
{
  TF_class *ret = new TF_class(
    super? super->clone() : NULL,
    cloneASTList(ctors)
  );
  return ret;
}

DEFN_AST_DOWNCASTS(ToplevelForm, TF_option, TF_OPTION)

TF_option::~TF_option()
{
  while (args.isNotEmpty()) {
    args.removeFirst();
  }
}

void TF_option::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TF_option);

  ToplevelForm::debugPrint(os, indent, subtreeName);

  PRINT_STRING(name);
  PRINT_LIST(sm_string, args);
}

TF_option *TF_option::clone() const
{
  TF_option *ret = new TF_option(
    name,
    shallowCloneASTList(args)
  );
  return ret;
}

DEFN_AST_DOWNCASTS(ToplevelForm, TF_enum, TF_ENUM)

TF_enum::~TF_enum()
{
  while (enumerators.isNotEmpty()) {
    enumerators.removeFirst();
  }
}

void TF_enum::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, TF_enum);

  ToplevelForm::debugPrint(os, indent, subtreeName);

  PRINT_STRING(name);
  PRINT_LIST(sm_string, enumerators);
}

TF_enum *TF_enum::clone() const
{
  TF_enum *ret = new TF_enum(
    name,
    shallowCloneASTList(enumerators)
  );
  return ret;
}


// ------------------ ASTClass -------------------
// *** DO NOT EDIT ***
ASTClass::~ASTClass()
{
  args.deleteAll();
  bases.deleteAll();
  decls.deleteAll();
}

void ASTClass::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, ASTClass);

  PRINT_STRING(name);
  PRINT_LIST(CtorArg, args);
  PRINT_LIST(BaseClass, bases);
  PRINT_LIST(Annotation, decls);
}

ASTClass *ASTClass::clone() const
{
  ASTClass *ret = new ASTClass(
    name,
    cloneASTList(args),
    cloneASTList(bases),
    cloneASTList(decls)
  );
  return ret;
}


// ------------------ AccessMod -------------------
// *** DO NOT EDIT ***
AccessMod::~AccessMod()
{
  while (mods.isNotEmpty()) {
    mods.removeFirst();
  }
}

void AccessMod::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, AccessMod);

  PRINT_GENERIC(acc);
  PRINT_LIST(sm_string, mods);
}

AccessMod *AccessMod::clone() const
{
  AccessMod *ret = new AccessMod(
    acc,
    shallowCloneASTList(mods)
  );
  return ret;
}


// ------------------ Annotation -------------------
// *** DO NOT EDIT ***
Annotation::~Annotation()
{
}

char const * const Annotation::kindNames[Annotation::NUM_KINDS] = {
  "UserDecl",
  "CustomCode",
};

void Annotation::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
}

DEFN_AST_DOWNCASTS(Annotation, UserDecl, USERDECL)

UserDecl::~UserDecl()
{
  delete amod;
}

void UserDecl::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, UserDecl);

  Annotation::debugPrint(os, indent, subtreeName);

  PRINT_SUBTREE(amod);
  PRINT_STRING(code);
  PRINT_STRING(init);
}

UserDecl *UserDecl::clone() const
{
  UserDecl *ret = new UserDecl(
    amod? amod->clone() : NULL,
    code,
    init
  );
  return ret;
}

DEFN_AST_DOWNCASTS(Annotation, CustomCode, CUSTOMCODE)

CustomCode::~CustomCode()
{
}

void CustomCode::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, CustomCode);

  Annotation::debugPrint(os, indent, subtreeName);

  PRINT_STRING(qualifier);
  PRINT_STRING(code);
}

CustomCode *CustomCode::clone() const
{
  CustomCode *ret = new CustomCode(
    qualifier,
    code
  );
  return ret;
}


// ------------------ CtorArg -------------------
// *** DO NOT EDIT ***
CtorArg::~CtorArg()
{
}

void CtorArg::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, CtorArg);

  PRINT_BOOL(isOwner);
  PRINT_STRING(type);
  PRINT_STRING(name);
  PRINT_STRING(defaultValue);
}

CtorArg *CtorArg::clone() const
{
  CtorArg *ret = new CtorArg(
    isOwner,
    type,
    name,
    defaultValue
  );
  return ret;
}


// ------------------ BaseClass -------------------
// *** DO NOT EDIT ***
BaseClass::~BaseClass()
{
}

void BaseClass::debugPrint(std::ostream &os, int indent, char const *subtreeName) const
{
  PRINT_HEADER(subtreeName, BaseClass);

  PRINT_GENERIC(access);
  PRINT_STRING(name);
}

BaseClass *BaseClass::clone() const
{
  BaseClass *ret = new BaseClass(
    access,
    name
  );
  return ret;
}


// *** DO NOT EDIT ***


#include "sm_strutil.h"

sm_string toString(AccessCtl acc)
{
  char const *arr[] = {
    "public",
    "private",
    "protected",
    "ctor",
    "dtor",
    "pure_virtual"
  };
  STATIC_ASSERT(TABLESIZE(arr) == NUM_ACCESSCTLS);
  xassert((unsigned)acc < NUM_ACCESSCTLS);
  return sm_string(arr[acc]);
}

sm_string ASTClass::classKindName() const
{
  sm_string ret = sm_stringToupper(name);
  if (ret == name) {
    // this simplemindedly avoids collisions with itself, and I think
    // it even avoids collisions with other classes, since if they would
    // collide with this, they'd collide with themselves too, and hence
    // get an extra "KIND_" prepended..
    ret &= "KIND_";
  }
  return ret;
}

bool AccessMod::hasMod(char const *mod) const
{
  FOREACH_ASTLIST(sm_string, mods, iter) {
    if (iter.data()->equals(mod)) {
      return true;
    }
  }
  return false;      // not found
}



