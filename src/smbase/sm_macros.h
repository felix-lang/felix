// macros.h            see license.txt for copyright and terms of use
// grab-bag of useful macros, stashed here to avoid mucking up
//   other modules with more focus; there's no clear rhyme or
//   reason for why some stuff is here and some in typ.h
// (no configuration stuff here!)

#ifndef __MACROS_H
#define __MACROS_H

#include "sm_typ.h"

// complement of ==
#define NOTEQUAL_OPERATOR(T)             \
  bool operator != (T const &obj) const  \
    { return !operator==(obj); }

// toss this into a class that already has == and < defined, to
// round out the set of relational operators (assumes a total
// order, i.e.  a < b  <=>  b < a)
#define RELATIONAL_OPERATORS(T)                    \
  NOTEQUAL_OPERATOR(T)                             \
  bool operator <= (T const &obj) const            \
    { return !obj.operator<(*this); }              \
  bool operator > (T const &obj) const             \
    { return obj.operator<(*this); }               \
  bool operator >= (T const &obj) const            \
    { return !operator<(obj); }


// member copy in constructor initializer list
#define DMEMB(var) var(obj.var)

// member copy in operator =
#define CMEMB(var) var = obj.var

// member comparison in operator ==
#define EMEMB(var) var == obj.var


// standard insert operator
// (note that you can put 'virtual' in front of the macro call if desired)
#define INSERT_OSTREAM(T)                                \
  void insertOstream(std::ostream &os) const;                 \
  friend std::ostream& operator<< (std::ostream &os, T const &obj) \
    { obj.insertOstream(os); return os; }


// usual declarations for a data object (as opposed to control object)
#define DATA_OBJ_DECL(T)                \
  T();                                  \
  T(T const &obj);                      \
  ~T();                                 \
  T& operator= (T const &obj);          \
  bool operator== (T const &obj) const; \
  NOTEQUAL_OPERATOR(T)                  \
  INSERTOSTREAM(T)


// copy this to the .cc file for implementation of DATA_OBJ_DECL
#if 0
T::T()
{}

T::T(T const &obj)
  : DMEMB(),
    DMEMB(),
    DMEMB()
{}

T::~T()
{}

T& T::operator= (T const &obj)
{
  if (this != &obj) {
    CMEMB();
  }
  return *this;
}

bool T::operator== (T const &obj) const
{
  return
    EMEMB() &&
    EMEMB();
}

void T::insertOstream(std::ostream &os) const
{}
#endif // 0


// assert something at compile time (must use this inside a function);
// works because compilers won't let us declare negative-length arrays
// (the expression below works with egcs-1.1.2, gcc-2.x, gcc-3.x)
#define STATIC_ASSERT(cond) \
  { (void)((int (*)(char failed_static_assertion[(cond)?1:-1]))0); }

// assert that a table is an expected size; the idea is to make sure
// that static data in some table gets updated when a corresponding
// symbolic constant is changed
#define ASSERT_TABLESIZE(table, size) \
  STATIC_ASSERT(TABLESIZE(table) == (size))


// for silencing variable-not-used warnings
template <class T>
inline void pretendUsedFn(T const &) {}
#define PRETEND_USED(arg) pretendUsedFn(arg) /* user ; */


// appended to function declarations to indicate they do not
// return control to their caller; e.g.:
//   void exit(int code) NORETURN;
#ifdef __GNUC__
  #define NORETURN __attribute__((noreturn))
#else
  // just let the warnings roll if we can't suppress them
  #define NORETURN
#endif


// these two are a common idiom in my code for typesafe casts;
// they are essentially a roll-your-own RTTI
#define CAST_MEMBER_FN(destType)                                                \
  destType const &as##destType##C() const;                                      \
  destType &as##destType() { return const_cast<destType&>(as##destType##C()); }

#define CAST_MEMBER_IMPL(inClass, destType)         \
  destType const &inClass::as##destType##C() const  \
  {                                                 \
    xassert(is##destType());                        \
    return (destType const&)(*this);                \
  }


// same as the above, but returning pointers; I think returning
// references was a mistake
#define DOWNCAST_FN(destType)                                                   \
  destType const *as##destType##C() const;                                      \
  destType *as##destType() { return const_cast<destType*>(as##destType##C()); }

#define DOWNCAST_IMPL(inClass, destType)            \
  destType const *inClass::as##destType##C() const  \
  {                                                 \
    xassert(is##destType());                        \
    return static_cast<destType const*>(this);      \
  }


// keep track of a count and a high water mark
#define INC_HIGH_WATER(count, highWater)  \
  count++;                                \
  if (count > highWater) {                \
    highWater = count;                    \
  }


// egcs has the annoying "feature" that it warns
// about switches on enums where not all cases are
// covered .... what is this, f-ing ML??
#define INCL_SWITCH \
  default: break; /*silence warning*/


// for a class that maintains allocated-node stats
#define ALLOC_STATS_DECLARE                     \
  static int numAllocd;                         \
  static int maxAllocd;                         \
  static void printAllocStats(bool anyway);

// these would go in a .cc file, whereas above goes in .h file
#define ALLOC_STATS_DEFINE(classname)                      \
  int classname::numAllocd = 0;                            \
  int classname::maxAllocd = 0;                            \
  STATICDEF void classname::printAllocStats(bool anyway)   \
  {                                                        \
    if (anyway || numAllocd != 0) {                        \
      std::cout << #classname << " nodes: " << numAllocd        \
           << ", max  nodes: " << maxAllocd                \
           << std::endl;                                        \
    }                                                      \
  }

#define ALLOC_STATS_IN_CTOR                     \
  INC_HIGH_WATER(numAllocd, maxAllocd);

#define ALLOC_STATS_IN_DTOR                     \
  numAllocd--;


// ----------- automatic data value restorer -------------
// used when a value is to be set to one thing now, but restored
// to its original value on return (even when the return is by
// an exception being thrown)
template <class T>
class Restorer {
  T &variable;
  T prevValue;

public:
  Restorer(T &var, T newValue)
    : variable(var),
      prevValue(var)
  {
    variable = newValue;
  }

  // this one does not set it to a new value, just remembers the current
  Restorer(T &var)
    : variable(var),
      prevValue(var)
  {}

  ~Restorer()
  {
    variable = prevValue;
  }
};


// declare a bunch of a set-like operators for enum types
#define ENUM_BITWISE_AND(Type)                  \
  inline Type operator& (Type f1, Type f2)      \
    { return (Type)((int)f1 & (int)f2); }       \
  inline Type& operator&= (Type &f1, Type f2)   \
    { return f1 = f1 & f2; }

#define ENUM_BITWISE_OR(Type)                   \
  inline Type operator| (Type f1, Type f2)      \
    { return (Type)((int)f1 | (int)f2); }       \
  inline Type& operator|= (Type &f1, Type f2)   \
    { return f1 = f1 | f2; }

#define ENUM_BITWISE_XOR(Type)                  \
  inline Type operator^ (Type f1, Type f2)      \
    { return (Type)((int)f1 ^ (int)f2); }       \
  inline Type& operator^= (Type &f1, Type f2)   \
    { return f1 = f1 ^ f2; }

#define ENUM_BITWISE_NOT(Type, ALL)             \
  inline Type operator~ (Type f)                \
    { return (Type)((~(int)f) & ALL); }

#define ENUM_BITWISE_OPS(Type, ALL)             \
  ENUM_BITWISE_AND(Type)                        \
  ENUM_BITWISE_OR(Type)                         \
  ENUM_BITWISE_XOR(Type)                        \
  ENUM_BITWISE_NOT(Type, ALL)


// macro to conditionalize something on NDEBUG; I typically use this
// to hide the declaration of a variable whose value is only used by
// debugging trace statements (and thus provokes warnings about unused
// variables if NDEBUG is set)
#ifdef NDEBUG
  #define IFDEBUG(stuff)
#else
  #define IFDEBUG(stuff) stuff
#endif


// put at the top of a class for which the default copy ctor
// and operator= are not desired; then don't define these functions
#define NO_OBJECT_COPIES(name)   \
  private:                       \
    name(name&);                 \
    void operator=(name&) /*user ;*/


#endif // __MACROS_H
