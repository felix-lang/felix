#include <stdio.h>

#include "flx_exceptions.hpp"

namespace flx { namespace rtl {
// ********************************************************
// standard exceptions -- implementation
// ********************************************************
flx_exception_t::~flx_exception_t(){}

flx_exec_failure_t::flx_exec_failure_t(::std::string f, ::std::string o, ::std::string w) :
  filename(f),
  operation(o),
  what(w)
{}

flx_out_of_memory_t::flx_out_of_memory_t(){}
flx_out_of_memory_t::~flx_out_of_memory_t(){}
flx_exec_failure_t::~flx_exec_failure_t(){}

flx_range_srcref_t::flx_range_srcref_t() :
    filename(""),startline(0),startcol(0),endline(0),endcol(0){}
flx_range_srcref_t::flx_range_srcref_t(char const *f,int sl, int sc, int el, int ec) :
    filename(f),startline(sl),startcol(sc),endline(el),endcol(ec){}

flx_halt_t::flx_halt_t(flx_range_srcref_t ff, char const *cf, int cl, ::std::string r) :
   reason(r), flx_loc(ff), cxx_srcfile(cf), cxx_srcline(cl) {}
flx_halt_t::~flx_halt_t(){}

flx_match_failure_t::flx_match_failure_t(flx_range_srcref_t ff, char const *cf, int cl) :
   flx_loc(ff), cxx_srcfile(cf), cxx_srcline(cl) {}
flx_match_failure_t::~flx_match_failure_t(){}

flx_dropthru_failure_t::flx_dropthru_failure_t(flx_range_srcref_t ff, char const *cf, int cl) :
   flx_loc(ff), cxx_srcfile(cf), cxx_srcline(cl) {}
flx_dropthru_failure_t::~flx_dropthru_failure_t(){}

flx_assert_failure_t::flx_assert_failure_t(flx_range_srcref_t ff, char const *cf, int cl) :
   flx_loc(ff), cxx_srcfile(cf), cxx_srcline(cl) {}
flx_assert_failure_t::~flx_assert_failure_t(){}

flx_assert2_failure_t::flx_assert2_failure_t(flx_range_srcref_t ff, flx_range_srcref_t ff2, char const *cf, int cl) :
   flx_loc(ff), flx_loc2(ff2), cxx_srcfile(cf), cxx_srcline(cl) {}
flx_assert2_failure_t::~flx_assert2_failure_t(){}

flx_axiom_check_failure_t::flx_axiom_check_failure_t(flx_range_srcref_t ff, flx_range_srcref_t ff2, char const *cf, int cl) :
   flx_loc(ff), flx_loc2(ff2), cxx_srcfile(cf), cxx_srcline(cl) {}
flx_axiom_check_failure_t::~flx_axiom_check_failure_t(){}

flx_range_failure_t::flx_range_failure_t(long l, long x, long h, flx_range_srcref_t ff, char const *cf, int cl) :
   min(l), v(x), max(h), flx_loc(ff), cxx_srcfile(cf), cxx_srcline(cl) {}
flx_range_failure_t::~flx_range_failure_t(){}

flx_switch_failure_t::~flx_switch_failure_t(){}

flx_link_failure_t::flx_link_failure_t(::std::string f, ::std::string o, ::std::string w) :
  filename(f),
  operation(o),
  what(w)
{}

flx_link_failure_t::~flx_link_failure_t(){}
flx_link_failure_t::flx_link_failure_t(){}


long range_check (long l, long x, long h, flx_range_srcref_t sref, char const *cf, int cl)
{
  if (x>=l && x<h) return x;
  throw flx::rtl::flx_range_failure_t (l,x,h,sref,cf,cl);
}

void print_loc(FILE *ef,flx_range_srcref_t x,char const *cf, int cl)
{
  fprintf(ef,"Felix location: %s %d[%d]-%d[%d]\n",
    x.filename,
    x.startline,
    x.startcol,
    x.endline,
    x.endcol
  );
  fprintf(ef,"C++ location  : %s %d\n", cf, cl);
}

}}
