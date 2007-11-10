// autofile.h            see license.txt for copyright and terms of use
// little wrapper around FILE*

#ifndef SMUTIL_H
#define SMUTIL_H

#include <stdio.h>      // FILE


// fopen, but throw an XOpen exception (see exc.h) on failure instead
// of returning NULL
FILE *xfopen(char const *fname, char const *mode);


// automatically close a file in the destructor
class AutoFclose {
private:       // data
  FILE *fp;

private:       // disallowed
  AutoFclose(AutoFclose&);
  void operator=(AutoFclose&);

public:
  AutoFclose(FILE *f) : fp(f) {}
  ~AutoFclose() { fclose(fp); }

  // may as well allow access to my storage
  FILE *getFP() { return fp; }
};


// simple wrapper on FILE*
class AutoFILE : private AutoFclose {
public:
  // open, throwing an XOpen exception on failure
  AutoFILE(char const *fname, char const *mode);

  // close the file
  ~AutoFILE();

  // behave like FILE* in between
  operator FILE* () { return getFP(); }
};


#endif // SMUTIL_H
