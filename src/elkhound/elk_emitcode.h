// emitcode.h            see license.txt for copyright and terms of use
// track state of emitted code so I can emit #line too

#ifndef EMITCODE_H
#define EMITCODE_H

#include <fstream>      // std::ofstream
#include "sm_str.h"
#include "sm_srcloc.h"

class EmitCode : public sm_stringBuilder {
private:     // data
  std::ofstream os;         // stream to write to
  sm_string fname;        // filename for emitting #line
  int line;            // current line number

public:      // funcs
  EmitCode(char const *fname);
  ~EmitCode();

  sm_string const &getFname() const { return fname; }

  // get current line number; flushes internally
  int getLine();

  // flush data in sm_stringBuffer to 'os'
  void flush();
};


// return a #line directive for the given location
sm_string lineDirective(SourceLoc loc);

// emit a #line directive to restore reporting to the
// EmitCode file itself (the 'sb' argument must be an EmitFile object)
sm_stringBuilder &restoreLine(sm_stringBuilder &sb);


#endif // EMITCODE_H
