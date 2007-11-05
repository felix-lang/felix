#line 3236 "./lpsrc/flx_rtl.pak"
#include "flx_strutil.hpp"

namespace flx { namespace rtl { namespace strutil {

  string atostr(char const *a) {
    if(a) return a;
    else return "";
  }

#ifdef FLX_HAVE_VSNPRINTF
  string flx_asprintf(char const *fmt,...){
    va_list ap;
    va_start(ap,fmt);
    //printf("vsnprintf TRIAL\n");
    int n = vsnprintf(NULL,0,fmt,ap);
    //printf("vsnprintf size=%d\n",n);
    va_end(ap);
    char *res = (char*)malloc(n+1);
    va_start(ap,fmt);
    vsnprintf(res,n+1,fmt,ap);
    va_end(ap);
    string s = string(res);
    free(res);
    return s;
  }
#else
  // THIS IS UNSAFE .. but Windows sucks.
  // It documents vsnprintf .. but doesn't provide it
  string flx_asprintf(char const *fmt,...){
    //printf("vsnprintf EMULATION!\n");
    va_list ap;
    int n = 10000; // hack, WILL crash if not enough
    char *res = (char*)malloc(n+1);
    va_start(ap,fmt);
    vsprintf(res,fmt,ap);
    va_end(ap);
    string s = string(res);
    free(res);
    return s;
  }
#endif

}}}
