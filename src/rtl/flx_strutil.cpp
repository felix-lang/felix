#include <stdio.h>

#include "flx_strutil.hpp"

namespace flx { namespace rtl { namespace strutil {

  string atostr(char const *a) {
    if(a) return a;
    else return "";
  }

  string flxid_to_cid (string const &s)
  {
    string out = "";
    int n = s.size();
    // leading digit
    if (n > 1 && s[0] >= '0' && s[0] <= '9') out += "_";
    for (int i = 0; i < n; ++i)
    {
      char ch = s[i];
      /* from http://www.w3.org/TR/html4/sgml/entities.html */
      switch (ch)
      {
        case ' ': out += "__sp_"; break;
        case '!': out += "__excl_"; break;
        case '"': out += "__quot_"; break;
        case '#': out += "__num_"; break;
        case '$': out += "__dollar_"; break;
        case '%': out += "__percnt_"; break;
        case '&': out += "__amp_"; break;
        case '\'':  out +=  "__apos_"; break;
        case '(': out += "__lpar_"; break;
        case ')': out += "__rpar_"; break;
        case '*': out += "__ast_"; break;
        case '+': out += "__plus_"; break;
        case ',': out += "__comma_"; break;
        case '-': out += "__hyphen_"; break;
        case '.': out += "__period_"; break;
        case '/': out += "__sol_"; break;
        case ':': out += "__colon_"; break;
        case ';': out += "__semi_"; break;
        case '<': out += "__lt_"; break;
        case '=': out += "__equals_"; break;
        case '>': out += "__gt_"; break;
        case '?': out += "__quest_"; break;
        case '@': out += "__commat_"; break;
        case '[': out += "__lsqb_"; break;
        case '\\': out += "__bsol_"; break;
        case ']': out += "__rsqb_"; break;
        case '^': out += "__caret_"; break;
        case '`': out += "__grave_"; break;
        case '{': out += "__lcub_"; break;
        case '|': out += "__verbar_"; break;
        case '}': out += "__rcub_"; break;
        case '~': out += "__tilde_"; break;
        default: out += string (1,ch);
      }
   }
   return out;
  }

  string chop_extension (string const &s)
  {
     int n = s.size();
     for(int i = n - 1; i >= 0; --i) 
     {
       if (s[i] == '/') return s;
       if (s[i] == '\\') return s;
       if (s[i] == '.') return s.substr(0,i);
     }
     return s;
  }

  string basename (string const &s) 
  {
     int n = s.size();
     for(int i = n - 1; i >= 0; --i) 
     {
       if (s[i] == '/') return s.substr (i+1,n-i);
       if (s[i] == '\\') return s.substr (i+1,n-i);
     }
     return s;
  }
  string filename_to_modulename (string const &s)
  {
     string a = basename (s);
     a = chop_extension (a);
     a = flxid_to_cid (a);
     return a; 
  }

#ifdef FLX_HAVE_VSNPRINTF
  string flx_asprintf(char const *fmt,...){
    va_list ap;
    va_start(ap,fmt);
    //printf("vsnprintf TRIAL\n");
    int n = vsnprintf(NULL,0,fmt,ap);
    //printf("vsnprintf size=%d\n",n);
    va_end(ap);
    char *res = new char[n + 1];
    va_start(ap,fmt);
    vsnprintf(res,n+1,fmt,ap);
    va_end(ap);
    string s = string(res);
    delete [] res;
    return s;
  }
#else
  // THIS IS UNSAFE .. but Windows sucks.
  // It documents vsnprintf .. but doesn't provide it
  string flx_asprintf(char const *fmt,...){
    //printf("vsnprintf EMULATION!\n");
    va_list ap;
    int n = 10000; // hack, WILL crash if not enough
    char *res = new char[n+1];
    va_start(ap,fmt);
    vsprintf(res,fmt,ap);
    va_end(ap);
    string s = string(res);
    delete [] res;
    return s;
  }
#endif

}}}
