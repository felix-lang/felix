// exc.cc            see license.txt for copyright and terms of use
// code for exc.h
// Scott McPeak, 1996-1998  This file is public domain.

#include "sm_exc.h"

#include <cstring>       // std::strlen, std::strcpy
#include <iostream>     // clog
#include <stdarg.h>       // va_xxx
#include <ctype.h>        // toupper, tolower


// ------------------------- xBase -----------------
bool xBase::logExceptions = true;
int xBase::creationCount = 0;


xBase::xBase(char const *m)
  : msg(m)
{
  if (logExceptions) {
    std::clog << "Exception thrown: " << m << std::endl;
  }

  // done at very end when we know this object will
  // successfully be created
  creationCount++;
}


xBase::xBase(xBase const &obj)
  : msg(obj.msg)
{
  creationCount++;
}


xBase::~xBase()
{
  creationCount--;
}


// this is obviously not perfect, since exception objects can be
// created and not thrown; I heard the C++ standard is going to,
// or already does, include (by this name?) a function that does this
// correctly; until then, this will serve as a close approximation
// (this kind of test is, IMO, not a good way to handle the underlying
// problem, but it does reasonably handle 70-90% of the cases that
// arise in practice, so I will endorse it for now)
bool unwinding()
{
  return xBase::creationCount != 0;
}


// tweaked version
bool unwinding_other(xBase const &)
{
  // we know the passed xBase exists.. any others?
  return xBase::creationCount > 1;
}


void xBase::insert(std::ostream &os) const
{
  os << why();
}


void xbase(char const *msg)
{
  xBase x(msg);
  THROW(x);
}


// ------------------- x_assert -----------------
x_assert::x_assert(char const *cond, char const *fname, int line)
  : xBase(sm_stringb(
      "Assertion failed: " << cond <<
      ", file " << fname <<
      " line " << line)),
    condition(cond),
    filename(fname),
    lineno(line)
{}

x_assert::x_assert(x_assert const &obj)
  : xBase(obj),
    condition(obj.condition),
    filename(obj.filename),
    lineno(obj.lineno)
{}

x_assert::~x_assert()
{}


// failure function, declared in xassert.h
void x_assert_fail(char const *cond, char const *file, int line)
{
  THROW(x_assert(cond, file, line));
}


// --------------- xFormat ------------------
xFormat::xFormat(char const *cond)
  : xBase(sm_stringb("Formatting error: " << cond)),
    condition(cond)
{}

xFormat::xFormat(xFormat const &obj)
  : xBase(obj),
    condition(obj.condition)
{}

xFormat::~xFormat()
{}


void xformat(char const *condition)
{
  xFormat x(condition);
  THROW(x);
}

void formatAssert_fail(char const *cond, char const *file, int line)
{
  xFormat x(sm_stringc << "format assertion failed, "
                    << file << ":" << line << ": "
                    << cond);
  THROW(x);
}


// -------------------- XOpen -------------------
XOpen::XOpen(char const *fname)
  : xBase(sm_stringc << "failed to open file: " << fname),
    filename(fname)
{}

XOpen::XOpen(XOpen const &obj)
  : xBase(obj),
    DMEMB(filename)
{}

XOpen::~XOpen()
{}


void throw_XOpen(char const *fname)
{
  XOpen x(fname);
  THROW(x);
}


// ---------------- test code ------------------
#ifdef TEST_EXC

int main()
{
  xBase x("yadda");
  std::cout << x << std::endl;

  try {
    THROW(x);
  }
  catch (xBase &x) {
    std::cout << "caught xBase: " << x << std::endl;
  }

  return 0;
}

#endif // TEST_EXC

