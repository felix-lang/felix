#include <cstdio>
#include <cstring>
#include <string>
#include "flx_ioutil.hpp"
namespace flx { namespace rtl { namespace ioutil {
  using namespace std;

/* small buffer for testing, should be much large in production version */
#define MYBUFSIZ 512
  string load_file (FILE *fi)
  {
    if (fi)
    {
      string x = "";
      char buffer[MYBUFSIZ];
more:
      int n = fread(buffer,1,MYBUFSIZ,fi);
      if(n>0) x = x + string(buffer,n);
      if (n == MYBUFSIZ)goto more;
      fclose(fi);
      return x;
    }
    else return "";
  }

  string load_file (string f)
  {
    char const *fname = f.data();
    FILE *fi = fopen(fname,"rb"); // note: binary mode!
    if (fi)
    {
      string x = "";
      char buffer[MYBUFSIZ];
more:
      int n = fread(buffer,1,MYBUFSIZ,fi);
      if(n>0) x = x + string(buffer,n);
      if(n == MYBUFSIZ)goto more;
      fclose(fi);
      return x;
    }
    else return "";
  }

  // includes ewline if present
  // null string indicates end of file
  string readln (FILE *fi)
  {
    if(fi)
    {
      string x = "";
      char buffer[MYBUFSIZ+1];
      buffer[MYBUFSIZ]='\0';
      int n;
      while
      (
        !(
          (n=x.size()) &&
          x[n-1]=='\n'
        )
        &&
        fgets(buffer,MYBUFSIZ,fi)
      )
        x = x + string(buffer);
      return x;
    }
    else return "";
  }

  void write (FILE *fi, string s)
  {
    fwrite(s.data(),s.size(),1,fi);
  }

  void writeln (FILE *fi, string s)
  {
    static char *eol = "\n";
    static int n = 0;
    if(n==0)n=strlen(eol);
    fwrite(s.data(),s.size(),1,fi);
    fwrite(eol,n,1,fi);
  }

}}}
