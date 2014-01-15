#include <cstdio>
#include <cstring>
#include <string>
#include <iostream>
#include <cassert>
#include "flx_ioutil.hpp"

#if FLX_WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

namespace flx { namespace rtl { namespace ioutil {
  using namespace std;


#if FLX_WIN32
  int flx_fileno (FILE *f) { return _fileno (f); }
  bool flx_isatty(int fd) { return 1 == _isatty (fd); }
#else
  int flx_fileno (FILE *f) { return fileno (f); }
  bool flx_isatty(int fd) { return 1 == isatty (fd); }
#endif

  bool flx_isatty (FILE *f) 
  {
    return 1 == flx_isatty (flx_fileno (f));
  }

  bool flx_isstdin (FILE *f)
  {
    return flx_fileno (f) == 0;
  }

  bool flx_isconsole (FILE *f)
  {
    return flx_isstdin (f) && flx_isatty(f);
  }


/* small buffer for testing, should be much large in production version */
#define MYBUFSIZ 5120

  string load_file (string f)
  {
    char const *fname = f.c_str();

    FILE *fi = fopen(fname,"rb"); // note: binary mode!

    if (fi)
    {
      string x = "";
      char buffer[MYBUFSIZ];
      while (!feof(fi)) {
        int n = fread(buffer,1,MYBUFSIZ,fi);
        if(n>0) x = x + string(buffer,n);
        else break;
      }
      fclose(fi);
      return x;
    }
    else return "";
  }

// C FILE IO

  string load_file (FILE *fi) // note does NOT close file! (would screw up popen)
  {
    if (fi)
    {
      string x = "";
      char buffer[MYBUFSIZ];
      while (!feof(fi)) {
        int n = fread(buffer,1,MYBUFSIZ,fi);
        if(n>0) x = x + string(buffer,n);
        else break;
      }
      return x;
    }
    else return "";
  }

  // includes newline if present
  // null string indicates end of file
  string raw_readln (FILE *fi)
  {
    if(fi)
    {
      string x = "";
      char buffer[MYBUFSIZ+1];
      buffer[MYBUFSIZ]='\0';
next:
      bool eof = fgets(buffer, MYBUFSIZ, fi) == 0;
      if(eof) return x;
      x = x + string(buffer);
      if(x[x.size()-1]=='\n') return x;
      goto next;
    }
    else return "";
  }

  string echo_readln (FILE *f)
  {
    string result = raw_readln (f);
    printf ("%s",result.c_str());
    return result;
  }

  string readln (FILE *f) { 
    bool doecho = flx_isstdin(f) && !flx_isatty (f);
    if (doecho)
       return echo_readln(f);
    else
       return raw_readln (f);
  }

  void write (FILE *fi, string s)
  {
    fwrite(s.data(),s.size(),1,fi);
  }

  static const char eol[] = { '\n' };

  void writeln (FILE *fi, string s)
  {
    fwrite(s.data(),s.size(),1,fi);
    fwrite(eol,sizeof(eol),1,fi);
  }

// C++ file IO

  string load_file (istream *fi) // note does NOT close file! (would screw up popen)
  {
    if (fi)
    {
      string x = "";
      char buffer[MYBUFSIZ];
more:
      fi->read(buffer,MYBUFSIZ);
      int n = fi->gcount();
      if(n>0) x = x + string(buffer,n);
      if (n == MYBUFSIZ)goto more;
      return x;
    }
    else return "";
  }

  // includes newline if present
  // null string indicates end of file
  string readln (istream *fi)
  {
    if(fi)
    {
      ::std::string x = "";
      ::std::getline(*fi,x);
      if (fi->fail()) return x; 
      else return x+"\n";
    }
    else return "";
  }

  void write (ostream *fi, string s)
  {
    fi->write(s.data(),s.size());
  }

  void writeln (ostream *fi, string s)
  {
    fi->write(s.data(),s.size());
    fi->write(eol,sizeof(eol));
  }



}}}
