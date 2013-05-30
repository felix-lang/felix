#ifndef __FLX_EXCEPTIONS_HPP__
#define __FLX_EXCEPTIONS_HPP__
#include "flx_exceptions_config.hpp"
#include <string>

namespace flx { namespace rtl {
// ********************************************************
// Standard C++ Exceptions
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_exception_t;
struct FLX_EXCEPTIONS_EXTERN flx_out_of_memory_t;
struct FLX_EXCEPTIONS_EXTERN flx_exec_failure_t;
struct FLX_EXCEPTIONS_EXTERN flx_range_srcref_t;
struct FLX_EXCEPTIONS_EXTERN flx_match_failure_t;
struct FLX_EXCEPTIONS_EXTERN flx_assert_failure_t;
struct FLX_EXCEPTIONS_EXTERN flx_assert2_failure_t;
struct FLX_EXCEPTIONS_EXTERN flx_axiom_check_failure_t;
struct FLX_EXCEPTIONS_EXTERN flx_switch_failure_t;
struct FLX_EXCEPTIONS_EXTERN flx_dropthru_failure_t;
struct FLX_EXCEPTIONS_EXTERN flx_link_failure_t;

// ********************************************************
/// EXCEPTION: Felix exception base abstraction.
/// Mainly used to convert catches into subroutine
/// calls which then dispatch on RTTI manually.
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_exception_t {
  virtual ~flx_exception_t()=0;
};

// ********************************************************
/// EXCEPTION: Out of Memory.
/// Thrown when out of memory or memory bound exceeded.
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_out_of_memory_t : flx_exception_t {
  flx_out_of_memory_t();
  virtual ~flx_out_of_memory_t();
};

// ********************************************************
/// EXCEPTION: EXEC protocol failure.
/// Thrown when trying to run a dead procedure
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_exec_failure_t : flx_exception_t {
  ::std::string filename;  ///< dll filename
  ::std::string operation; ///< faulty operation
  ::std::string what;      ///< error description
  flx_exec_failure_t(::std::string f, ::std::string o, ::std::string w);
  virtual ~flx_exec_failure_t();
};

// ********************************************************
/// SOURCE REFERENCE: to track places in user source code.
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_range_srcref_t {
  char const *filename;  ///< source file name
  int startline;   ///< first line (1 origin)
  int startcol;    ///< first column (1 origin)
  int endline;     ///< last line
  int endcol;      ///< last column
  flx_range_srcref_t(char const *f,int sl, int sc, int el, int ec);
  flx_range_srcref_t();
};

// ********************************************************
/// EXCEPTION: HALT.
/// Thrown by halt command
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_halt_t : flx_exception_t {
  ::std::string reason;         ///< halt argument
  flx_range_srcref_t flx_loc; ///< location in Felix file
  char const *cxx_srcfile;          ///< C++ file name
  int cxx_srcline;            ///< C++ line number
  flx_halt_t(flx_range_srcref_t ff, char const *cf, int cl, ::std::string reason);
  virtual ~flx_halt_t();
};

// ********************************************************
/// EXCEPTION: MATCH failure.
/// Thrown when no match cases match the argument of a match,
/// regmatch, or reglex
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_match_failure_t : flx_exception_t {
  flx_range_srcref_t flx_loc; ///< location in Felix file
  char const *cxx_srcfile;          ///< C++ file name
  int cxx_srcline;            ///< C++ line number
  flx_match_failure_t(flx_range_srcref_t ff, char const *cf, int cl);
  virtual ~flx_match_failure_t();
};

// ********************************************************
/// EXCEPTION: DROPTHRU failure.
/// Thrown when function drops off end without returning value
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_dropthru_failure_t : flx_exception_t {
  flx_range_srcref_t flx_loc; ///< location in Felix file
  char const *cxx_srcfile;          ///< C++ file name
  int cxx_srcline;            ///< C++ line number
  flx_dropthru_failure_t(flx_range_srcref_t ff, char const *cf, int cl);
  virtual ~flx_dropthru_failure_t();
};

// ********************************************************
/// EXCEPTION: ASSERT failure.
/// Thrown when user assertion fails
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_assert_failure_t : flx_exception_t {
  flx_range_srcref_t flx_loc; ///< location in Felix file
  char const *cxx_srcfile;          ///< C++ file
  int cxx_srcline;            ///< __LINE__ macro
  flx_assert_failure_t(flx_range_srcref_t ff, char const *cf, int cl);
  virtual ~flx_assert_failure_t();
};

struct FLX_EXCEPTIONS_EXTERN flx_assert2_failure_t : flx_exception_t {
  flx_range_srcref_t flx_loc; ///< location in Felix file
  flx_range_srcref_t flx_loc2; ///< second location in Felix file
  char const *cxx_srcfile;          ///< C++ file
  int cxx_srcline;            ///< __LINE__ macro
  flx_assert2_failure_t(flx_range_srcref_t ff, flx_range_srcref_t ff2, char const *cf, int cl);
  virtual ~flx_assert2_failure_t();
};

struct FLX_EXCEPTIONS_EXTERN flx_axiom_check_failure_t : flx_exception_t {
  flx_range_srcref_t flx_loc; ///< location in Felix file
  flx_range_srcref_t flx_loc2; ///< second location in Felix file
  char const *cxx_srcfile;          ///< C++ file
  int cxx_srcline;            ///< __LINE__ macro
  flx_axiom_check_failure_t (flx_range_srcref_t ff, flx_range_srcref_t ff2, char const *cf, int cl);
  virtual ~flx_axiom_check_failure_t ();
};

// ********************************************************
/// EXCEPTION: RANGE failure.
/// Thrown when a range check fails
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_range_failure_t : flx_exception_t {
  long min; long v; long max;
  flx_range_srcref_t flx_loc; ///< location in Felix file
  char const *cxx_srcfile;          ///< C++ file
  int cxx_srcline;            ///< __LINE__ macro
  flx_range_failure_t(long,long,long,flx_range_srcref_t ff, char const *cf, int cl);
  virtual ~flx_range_failure_t();
};

FLX_EXCEPTIONS_EXTERN long range_check (long l, long x, long h, flx_range_srcref_t sref, char const *cf, int cl);
FLX_EXCEPTIONS_EXTERN void print_loc(FILE *ef,flx_range_srcref_t x,char const *cf, int cl);


// ********************************************************
/// EXCEPTION: SWITCH failure. this is a system failure!
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_switch_failure_t : flx_exception_t {
  virtual ~flx_switch_failure_t();
};

// ********************************************************
/// EXCEPTION: DYNAMIC LINKAGE failure. this is a system failure!
// ********************************************************

struct FLX_EXCEPTIONS_EXTERN flx_link_failure_t : flx_exception_t {
  ::std::string filename;
  ::std::string operation;
  ::std::string what;
  flx_link_failure_t(::std::string f, ::std::string o, ::std::string w);
  flx_link_failure_t(); // unfortunately this one requires a default ctor.
  virtual ~flx_link_failure_t();
};

}}
#endif
