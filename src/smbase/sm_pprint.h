// pprint.h
// pretty-print code while emitting it



// NOTE: This module is a little simpler to use, but much less
// powerful than the 'boxprint' module.  I'm leaving this module
// here for now, but will probably delete it at some point.



// inspired by:
//   CIL's 'pretty' module
//   http://www.cs.berkeley.edu/~necula/cil/index.html
// and
//   Caml pretty-print module (boxes, etc.)
//   http://caml.inria.fr/FAQ/format-eng.html

// special characters:
//   '\n' - hard linebreak
//   '\r' - optional linebreak; is 1 space if the break isn't taken
//   '\b' - begin a break group (return to <here>)
//   '\a' - alternate begin group (return to <this_line_ind> + altIndent)
//   '\f' - finish a break group

#ifndef PPRINT_H
#define PPRINT_H

#include <iostream>      // std::ostream
#include "sm_str.h"
#include "sm_array.h"


// output interface for PPrint.. I'd like to just start using the
// C++ istd::ostreams interfaces, but reading on the net I get the
// impression they're still a little too much in flux
class PPrintOut {
public:
  virtual void write(char const *text) = 0;
  virtual ~PPrintOut(){}
};

class PPrintStringOut : public PPrintOut {
  sm_stringBuilder &sb;
public:
  PPrintStringOut(sm_stringBuilder &s) : sb(s) {}
  virtual void write(char const *text);
};

class PPrintOstreamOut : public PPrintOut {
  std::ostream &os;
public:
  PPrintOstreamOut(std::ostream &o) : os(o) {}
  virtual void write(char const *text);
};


// pretty printer formatting engine
class PPrint {
private:     // types
  // manages the line-setting algorithm
  class Setter {
  private:     // data
    // inter-line information
    PPrint &pprint;

    // emitted text in the current line
    sm_stringBuilder curLine;

    // indentation used for 'curLine'
    int curLineInd;

    // place in the 'line' buffer; all the chars up to this point
    // have been sent out
    int lineIndex;

    // stack of columns at which indent groups opened
    ArrayStack<int> indentGroups;

  private:     // funcs
    // add 'amt' spaces to 'curLine'
    void indent(int amt);

    // copy characters [lineIndex,lineIndex+p-1] from 'line' into
    // 'curLine', moving 'lineIndex' along so eventually it equals
    // 'p'; also maintain 'indentGroups'
    void emitTo(int p);

    // send all of 'curLine' to 'pprint.out', and clear 'curLine'
    void flush();

  public:      // funcs
    Setter(PPrint &p)
      : pprint(p),
        curLine(),
        curLineInd(0),
        lineIndex(0),
        indentGroups()
    {}
    ~Setter();

    void set();
  };
  friend class Setter;

private:     // data
  // the contents of each line, up to a hard linebreak, is accumulated here
  ArrayStack<char> line;

public:      // data
  // current indentation level for the beginning of a complete line
  // (one preceded by a hard linebreak)
  int lineIndent;

  // desired right margin; we'll try to set text so it doesn't go
  // beyond that many columns; defaults to 72
  int margin;

  // incremental indentation for '\a' groups; defaults to 2
  int altIndent;

  // if not NULL, text to emit at the start of every line; intended
  // for emitting text into a comment or other embedded context;
  // defaults to NULL; not counted against the margin
  char const *startText;

  // where to send output
  PPrintOut &out;

  // When true, and we find that the grouping is unbalanced at
  // the end of setting a line, pring a warning.  This defaults
  // to 'true'.  Note that while too many '\b's will only trigger
  // this warning, too many '\f's can cause an assertion failure
  // when the indentation stack underflows.
  static bool warnWhenUnbalanced;

private:     // funcs
  // take the current line buffer and break it up into output
  // lines, sending them to 'out'
  void set();

public:      // funcs
  PPrint(PPrintOut &out);
  ~PPrint();

  // basic printing routine; the text can contain the special
  // characters listed above; whenever a '\n' is seen, the current
  // line is set and emitted to 'out'
  void print(char const *text);

  // convenience
  PPrint& operator<< (int i);
  PPrint& operator<< (char const *s);

  // manage the line-start indentation
  void ind(int amt) { lineIndent += amt; }
};


class PPrintToString : public PPrint {
public:
  sm_stringBuilder sb;            // output (set) lines accumulate here
  PPrintStringOut sbOut;       // helper

public:
  PPrintToString()
    : PPrint(sbOut), sb(), sbOut(sb) {}
  ~PPrintToString();
};

class PPrintToOstream : public PPrint {
  PPrintOstreamOut osOut;

public:
  PPrintToOstream(std::ostream &os)
    : PPrint(osOut), osOut(os) {}
};


#endif // PPRINT_H
