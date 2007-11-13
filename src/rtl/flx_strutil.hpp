#ifndef __FLX_STRUTIL_HPP_
#define __FLX_STRUTIL_HPP_

#include <string>
#include <sstream>
#include <stdarg.h>

#include "flx_rtl_config.hpp"

//RF: was only to commented out to fix macosx problem,
//but lets see what happens to all the other builds.
//#ifndef MACOSX
//template class RTL_EXTERN std::basic_string<char>;
//#endif

namespace flx { namespace rtl { namespace strutil {
  using namespace std;
  template<class T>
  basic_string<T> mul(basic_string<T> s, int n) {
    basic_string<T> r = "";
    while(n--) r+=s;
    return r;
  }

  // normalise string positions Python style
  // note substr requires 0<=b<=size, 0<=n,
  // however n>size is OK
  template<class T>
  basic_string<T> substr(basic_string<T> const &s, int b, int e)
  {
    int n = s.size();
    if(b<0)  b=b+n;
    if(b<0)  b=0;
    if(b>=n) b=n;
    if(e<0)  e=e+n;
    if(e<0)  e=0;
    if(e>=n) e=n;
    int m =  e-b;
    if(m<0)  m=0;
    return s.substr(b,m);
  }

  template<class T>
  T subscript(basic_string<T> const &s, int i)
  {
    int n = s.size();
    if(i<0)  i=i+n;
    return i<0 || i >= n ? T(0) : s[i];
  }

  template<class T>
  string str(T const &t) {
    std::ostringstream x;
    x << t;
    return x.str();
  }

  RTL_EXTERN string atostr(char const *a);
  RTL_EXTERN string flx_asprintf(char const *fmt,...);

}}}

#endif

