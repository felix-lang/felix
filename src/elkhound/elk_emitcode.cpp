// emitcode.cc            see license.txt for copyright and terms of use
// code for emitcode.h

#include "elk_emitcode.h"
#include "sm_syserr.h"
#include "sm_srcloc.h"
#include "sm_trace.h"

EmitCode::EmitCode(char const *f)
  : sm_stringBuilder(),
    os(f),
    fname(f),
    line(1)
{
  if (!os) {
    xsyserror("open", fname);
  }
}

EmitCode::~EmitCode()
{
  flush();
}


int EmitCode::getLine()
{
  flush();
  return line;
}


void EmitCode::flush()
{
  // count newlines
  char const *p = pcharc();
  while (*p) {
    if (*p == '\n') {
      line++;
    }
    p++;
  }

  os << *this;
  setlength(0);
}


char const *hashLine()
{
  if (tracingSys("nolines")) {
    // emit with comment to disable its effect
    return "// #line ";
  }
  else {
    return "#line ";
  }
}


// note that #line must be preceeded by a newline
sm_string lineDirective(SourceLoc loc)
{
  char const *fname;
  int line, col;
  sourceLocManager->decodeLineCol(loc, fname, line, col);

  return sm_stringc << hashLine() << line << " \"" << fname << "\"\n";
}

sm_stringBuilder &restoreLine(sm_stringBuilder &sb)
{
  // little hack..
  EmitCode &os = (EmitCode&)sb;

  // +1 because we specify what line will be *next*
  int line = os.getLine()+1;
  return os << hashLine() << line
            << " \"" << os.getFname() << "\"\n";
}
