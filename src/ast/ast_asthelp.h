// asthelp.h            see license.txt for copyright and terms of use
// included by generated ast code

#ifndef ASTHELP_H
#define ASTHELP_H

#include "sm_astlist.h"
#include "ast_fakelist.h"
#include "sm_str.h"
#include "ast_locstr.h"

#include <iostream>    // ostream

// ----------------- downcasts --------------------
// the 'if' variants return NULL if the type isn't what's expected;
// the 'as' variants throw an exception in that case
#define DECL_AST_DOWNCASTS(type, tag)            \
  type const *if##type##C() const;               \
  type *if##type()                               \
    { return const_cast<type*>(if##type##C()); } \
  type const *as##type##C() const;               \
  type *as##type()                               \
    { return const_cast<type*>(as##type##C()); } \
  bool is##type() const                          \
    { return kind() == tag; }


#define DEFN_AST_DOWNCASTS(superclass, type, tag)\
  type const *superclass::if##type##C() const    \
  {                                              \
    if (kind() == tag) {                         \
      return (type const*)this;                  \
    }                                            \
    else {                                       \
      return NULL;                               \
    }                                            \
  }                                              \
                                                 \
  type const *superclass::as##type##C() const    \
  {                                              \
    xassert(kind() == tag);                      \
    return (type const*)this;                    \
  }


// ------------------- const typecase --------------------
#define ASTSWITCHC(supertype, nodeptr)           \
{                                                \
  supertype const *switch_nodeptr = (nodeptr);   \
  switch (switch_nodeptr->kind())

#define ASTCASEC(type, var)                           \
  case type::TYPE_TAG: {                              \
    type const *var = switch_nodeptr->as##type##C();

// the "1" versions mean "one argument", i.e. they
// do not bind a variable of the specified type
#define ASTCASEC1(type)                               \
  case type::TYPE_TAG: {

#define ASTNEXTC(type, var)                           \
    break;                                            \
  } /* end previous case */                           \
  case type::TYPE_TAG: {                              \
    type const *var = switch_nodeptr->as##type##C();

#define ASTNEXTC1(type)                               \
    break;                                            \
  } /* end previous case */                           \
  case type::TYPE_TAG: {

// end a case, and add an empty 'default' construct
#define ASTENDCASECD                                  \
    break;                                            \
  } /* end final case */                              \
  default: ;    /* silence warning */                 \
} /* end scope started before switch */

#define ASTDEFAULTC                                   \
    break;                                            \
  } /* end final case */                              \
  default: {

// end a case where an explicit default was present, or
// there is no need to add one (e.g. because it was exhaustive)
#define ASTENDCASEC                                   \
    break;                                            \
  } /* end final case */                              \
} /* end scope started before switch */


// ------------------- non-const typecase --------------------
#define ASTSWITCH(supertype, nodeptr)            \
{                                                \
  supertype *switch_nodeptr = (nodeptr);         \
  switch (switch_nodeptr->kind())

#define ASTCASE(type, var)                            \
  case type::TYPE_TAG: {                              \
    type *var = switch_nodeptr->as##type();

#define ASTCASE1(type)                                \
  case type::TYPE_TAG: {

#define ASTNEXT(type, var)                            \
    break;                                            \
  } /* end previous case */                           \
  case type::TYPE_TAG: {                              \
    type *var = switch_nodeptr->as##type();

#define ASTNEXT1(type)                                \
    break;                                            \
  } /* end previous case */                           \
  case type::TYPE_TAG: {

// end-of-switch behavior is same as in const case
#define ASTENDCASED ASTENDCASECD
#define ASTDEFAULT ASTDEFAULTC
#define ASTENDCASE ASTENDCASEC


// ------------------- debug print helpers -----------------
std::ostream &ind(std::ostream &os, int indent);

// I occasionally want to see addresses, so I just throw this
// switch and recompile..
#if 1
  // headers w/o addresses
  #define PRINT_HEADER(subtreeName, clsname)                 \
    ind(os, indent) << subtreeName << " = " #clsname ":\n";  \
    indent += 2   /* user ; */
#else
  // headers w/ addresses
  #define PRINT_HEADER(subtreeName, clsname)                                           \
    ind(os, indent) << subtreeName << " = " #clsname " (" << ((void*)this) << "):\n";  \
    indent += 2   /* user ; */
#endif


#define PRINT_STRING(var) \
  debugPrintStr(var, #var, os, indent)    /* user ; */

void debugPrintStr(sm_string const &s, char const *name,
                   std::ostream &os, int indent);


#define PRINT_LIST(T, list) \
  debugPrintList(list, #list, os, indent)     /* user ; */

template <class T>
void debugPrintList(ASTList<T> const &list, char const *name,
                    std::ostream &os, int indent)
{
  ind(os, indent) << name << ":\n";
  int ct=0;
  {
    FOREACH_ASTLIST(T, list, iter) {
      iter.data()->debugPrint(os, indent+2,
        sm_stringc << name << "[" << ct++ << "]");
    }
  }
}

// provide explicit specialization for sm_strings
void debugPrintList(ASTList<sm_string> const &list, char const *name,
                    std::ostream &os, int indent);
void debugPrintList(ASTList<LocString> const &list, char const *name,
                    std::ostream &os, int indent);


#define PRINT_FAKE_LIST(T, list) \
  debugPrintFakeList(list, #list, os, indent)     /* user ; */

template <class T>
void debugPrintFakeList(FakeList<T> const *list, char const *name,
                        std::ostream &os, int indent)
{
  ind(os, indent) << name << ":\n";
  int ct=0;
  {
    FAKELIST_FOREACH(T, list, iter) {
      iter->debugPrint(os, indent+2,
        sm_stringc << name << "[" << ct++ << "]");
    }
  }
}

// note that we never make FakeLists of sm_strings, since of course
// sm_strings do not have a 'next' pointer


#define PRINT_SUBTREE(tree)                     \
  if (tree) {                                   \
    (tree)->debugPrint(os, indent, #tree);      \
  }                                             \
  else {                                        \
    ind(os, indent) << #tree << " is null\n";   \
  } /* user ; (optional) */


#define PRINT_GENERIC(var) \
  ind(os, indent) << #var << " = " << ::toString(var) << "\n"   /* user ; */


#define PRINT_BOOL(var) \
  ind(os, indent) << #var << " = " << (var? "true" : "false") << "\n"   /* user ; */


// ------------------- xml print helpers -----------------
// dsw: given above in the debug print section.
//  ostream &ind(ostream &os, int indent);

#define XMLPRINT_HEADER(clsname)                            \
  ind(os, indent) << "<object type=\"" << #clsname "\">\n"; \
  indent += 2   /* user ; */                                \

#define XMLPRINT_FOOTER(clsname)                            \
  indent -= 2;                                              \
  ind(os, indent) << "</object>\n" /* user ; */

#define XMLPRINT_STRING(var)                                \
  xmlPrintStr(var, #var, os, indent) /* user ; */

void xmlPrintStr(sm_string const &s, char const *name,
                 std::ostream &os, int indent);


#define XMLPRINT_LIST(T, list)                              \
  xmlPrintList(list, #list, os, indent) /* user ; */

template <class T>
void xmlPrintList(ASTList<T> const &list, char const *name,
                  std::ostream &os, int indent)
{
  ind(os, indent) << "<member type=list name=\"" << name << "\">\n";
  {
    FOREACH_ASTLIST(T, list, iter) {
      iter.data()->xmlPrint(os, indent+2);
    }
  }
  ind(os, indent) << "</member>\n";
}

// provide explicit specialization for sm_strings
void xmlPrintList(ASTList<sm_string> const &list, char const *name,
                    std::ostream &os, int indent);
void xmlPrintList(ASTList<LocString> const &list, char const *name,
                    std::ostream &os, int indent);


#define XMLPRINT_FAKE_LIST(T, list) \
  xmlPrintFakeList(list, #list, os, indent)     /* user ; */

template <class T>
void xmlPrintFakeList(FakeList<T> const *list, char const *name,
                        std::ostream &os, int indent)
{
  ind(os, indent) << "<member type=fakelist name=\"" << name << "\">\n";
  {
    FAKELIST_FOREACH(T, list, iter) {
      iter->xmlPrint(os, indent+2);
    }
  }
  ind(os, indent) << "</member>\n";
}

// note that we never make FakeLists of sm_strings, since of course
// sm_strings do not have a 'next' pointer


#define XMLPRINT_SUBTREE(tree)                         \
  if (tree) {                                          \
    (tree)->xmlPrint(os, indent);                      \
  }                                                    \
  else {                                               \
    xassert(0); /* dsw:not sure what to do here yet */ \
    ind(os, indent) << #tree << " is null\n";          \
  } /* user ; (optional) */


// dsw: there's no way this can work in general
#define XMLPRINT_GENERIC(var)                                                         \
  ind(os, indent) << "<member type=generic name=\"" << #var << "\">\n";               \
  ind(os, indent+2) << "<object type=generic val=\"" << ::toString(var) << "\" />\n"; \
  ind(os, indent) << "</member>\n"   /* user ; */


#define XMLPRINT_BOOL(var)                                                                 \
  ind(os, indent) << "<member type=bool name=\"" << #var << "\">\n";                       \
  ind(os, indent+2) << "<object type=bool val=\"" << (var? "true" : "false") << "\" />\n"; \
  ind(os, indent) << "</member>\n"   /* user ; */


// ---------------------- deep-copy ------------------
// returns a new'd list because the AST node ctors want
// to accept an owner ptr to a list
template <class T>
ASTList<T> * /*owner*/ cloneASTList(ASTList<T> const &src)
{
  ASTList<T> *ret = new ASTList<T>;

  FOREACH_ASTLIST(T, src, iter) {
    ret->append(iter.data()->clone());
  }

  return ret;
}


// returns owner pointer to list of serfs.. using this isn't ideal
// because ASTList normally is owning, and probably deletes its
// elements in its destructor..
template <class T>
ASTList<T> * /*owner*/ shallowCloneASTList(ASTList<T> const &src)
{
  ASTList<T> *ret = new ASTList<T>;

  FOREACH_ASTLIST(T, src, iter) {
    // list backbone is const, but nodes' constness leaks away..
    ret->append(const_cast<T*>(iter.data()));
  }

  return ret;
}


// deep copy of a FakeList
template <class T>
FakeList<T> * /*owner*/ cloneFakeList(FakeList<T> const *src)
{
  if (!src) {
    return FakeList<T>::emptyList();     // base case of recursion
  }

  // clone first element
  T *head = src->firstC()->clone();
  xassert(head->next == NULL);     // it had better not copy the list tail itself!

  // attach to result of cloning the tail
  FakeList<T> *tail = cloneFakeList(src->butFirstC());
  return tail->prepend(head);
}


#endif // ASTHELP_H
