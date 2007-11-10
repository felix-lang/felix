// pprint.cc
// code for pprint.h

#include "sm_pprint.h"
#include "sm_breaker.h"
#include <stdio.h>       // sprintf
#include <cstring>      // std::memcpy


// ---------------------- PPrintOut ----------------------
void PPrintStringOut::write(char const *text)
{
  sb << text;
}

void PPrintOstreamOut::write(char const *text)
{
  os << text;
}


// ------------------------ PPrint ----------------------
bool PPrint::warnWhenUnbalanced = true;

PPrint::PPrint(PPrintOut &o)
  : line(),
    lineIndent(0),
    margin(72),
    altIndent(2),
    startText(NULL),
    out(o)
{}

PPrint::~PPrint()
{
  if (line.length() > 0) {
    // hit a breakpoint if we're in the debugger, since this
    // is unexpected
    breaker();

    // add a final newline to get all the output out
    print("\n");
  }
}


struct BreakInfo {
  int p;        // index in the 'line' array of the '\r'
  int pCol;     // column of emitted text where the newline would go
  int pInd;     // indent for the next line

  BreakInfo(int pp, int pc, int pi)
    : p(pp), pCol(pc), pInd(pi) {}
  BreakInfo() {}      // for use in arrays

  // when choosing breaks, we maximize this sum: the # of chars
  // that will be on this line, and the # of chars available to
  // fill on the next line
  int sum(/*int margin*/) const {
    // except, since all the margins play the same role and will
    // all cancel, we don't need to consider it
    return pCol +                 // this line
           (/*margin*/ - pInd);   // next line
  }

  // decision between two breaks
  bool betterThan(BreakInfo const &obj /*, int margin*/) {
    if (sum(/*margin*/) > obj.sum(/*margin*/))
      { return true; }

    // tiebreaker: prefer more space on the next line, since that's
    // likely to become the space available on the line after that,
    // etc.
    if (sum(/*margin*/) == obj.sum(/*margin*/) &&
        pInd < obj.pInd)
      { return true; }

    return false;
  }
};


void PPrint::Setter::indent(int amt)
{
  for (int i=0; i<amt; i++) {
    curLine << ' ';
  }
}


void PPrint::Setter::set()
{
  // initialize the indentation stack with the line-start indentation
  indentGroups.push(pprint.lineIndent);

  // loop over the actual emitted lines
  while (lineIndex < pprint.line.length()) {
    // indent the line by the amount at the top of the stack
    curLineInd = indentGroups.top();
    indent(curLineInd);

    // find all of the line breaks that would make the current line end
    // before the 'margin' column
    ArrayStack<BreakInfo> breaks;      // TODO: move up to avoid allocation

    // this will scan forward to find optional line breaks
    int p = lineIndex;

    // column at which the line would break, if we broke at 'p'
    int pCol = curLine.length();

    // the topmost entry of this stack says how far we'd indent
    // at the beginning of the next line, if we broke at 'p'
    ArrayStack<int> pInd;              // TODO: move up to avoid allocation

    // initialize 'pInd' with 'indentGroups', because what happens next
    // is we speculatively emit, as if doing it for real and consequently
    // updating 'indentGroups'
    {
      pInd.ensureAtLeast(indentGroups.length());
      for (int i=0; i < indentGroups.length(); i++) {
        pInd[i] = indentGroups[i];
      }
      pInd.setLength(indentGroups.length());
    }

    while (p < pprint.line.length()-1 && pCol < pprint.margin) {
      switch (pprint.line[p]) {
        case '\r':   // optional line break
          breaks.push(BreakInfo(p, pCol, pInd.top()));
          pCol++;    // if *not* taken, it will be a space
          break;

        case '\b':   // begin break group
          pInd.push(pCol);
          break;

        case '\a':   // alternate begin group
          pInd.push(curLineInd + pprint.altIndent);
          break;

        case '\f':   // break group end
          pInd.pop();
          break;

        default:     // printing character
          // increases output column
          pCol++;
          break;
      }

      p++;
    }

    if (pCol < pprint.margin) {
      // we got to the end of 'line' before hitting margin; emit the
      // remainder as-is
      emitTo(p+1 /*include newline*/);
      flush();
      return;
    }

    if (breaks.isEmpty()) {
      // no line breaks happen soon enough, so just find the first
      // break and take it
      while (p < pprint.line.length()-1) {
        if (pprint.line[p] == '\r') {
          emitTo(p /*not including '\r'*/);
          lineIndex++;     // skip '\r'
          curLine << '\n';
          flush();
          break;
        }

        p++;
      }

      if (p == pprint.line.length()-1) {
        // no optional line breaks at all; emit remainder
        emitTo(p+1 /*include newline*/);
        flush();
        return;
      }
    }

    else {
      // choose the best break from among those in 'breaks'
      int best = 0;
      for (int i=1; i < breaks.length(); i++) {
        if (breaks[i].betterThan(breaks[best] /*, pprint.margin*/)) {
          best = i;
        }
      }

      // break the line
      emitTo(breaks[best].p /*not including '\r'*/);
      lineIndex++;                  // skip '\r'
      curLine << '\n';
      flush();
    }
  }
}

PPrint::Setter::~Setter()
{
  if (indentGroups.length() != 1) {
    // unbalanced groups
    breaker();
    if (warnWhenUnbalanced) {
      std::cout << "warning: unbalanced indentation grouping in pprint input\n";
    }
  }
}


void PPrint::Setter::emitTo(int p)
{
  while (lineIndex < p) {
    char ch = pprint.line[lineIndex];
    switch (ch) {
      case '\r':   // optional line break
        // not taken, it's a space
        curLine << ' ';
        break;

      case '\b':   // begin break group
        indentGroups.push(curLine.length());
        break;

      case '\a':   // alternate begin group
        indentGroups.push(curLineInd + pprint.altIndent);
        break;

      case '\f':   // break group end
        indentGroups.pop();
        break;

      default:     // printing character
        curLine << ch;
        break;
    }

    lineIndex++;
  }
}


void PPrint::Setter::flush()
{
  if (pprint.startText) {
    pprint.out.write(pprint.startText);
  }
  pprint.out.write(curLine);
  curLine.clear();
}


void PPrint::set()
{
  // on entry, 'line' contains exactly one newline, at the end
  xassert(line[line.length()-1] == '\n');

  Setter s(*this);
  s.set();

  // clear the line, ready for the next one
  line.setLength(0);
}


void append(ArrayStack<char> &line, char const *src, int len)
{
  line.ensureAtLeast(line.length() + len);
  std::memcpy(line.getArrayNC()+line.length(),     // dest
         src, len);                           // src, len
  line.setLength(line.length() + len);
}

void PPrint::print(char const *text)
{
  // any newlines?
  char const *p = text;
  while (*p != 0) {
    if (*p == '\n') {
      // transfer everything up to the newline into the setting buffer
      int copylen = p-text+1;
      append(line, text, copylen);

      // advance 'text' so the next batch will begin after what we
      // just transferred
      text += copylen;

      // set the now-complete line
      set();
    }

    p++;
  }

  // copy the remainder into the line buffer
  append(line, text, p-text);
}


PPrint& PPrint::operator<< (int i)
{
  char tmp[40];
  sprintf(tmp, "%d", i);
  print(tmp);
  return *this;
}

PPrint& PPrint::operator<< (char const *s)
{
  print(s);
  return *this;
}


PPrintToString::~PPrintToString()
{}


// --------------------- test code -----------------------
#ifdef TEST_PPRINT

PPrintToString pp;

int main()
{
  pp.margin = 30;
  pp.startText = "; ";

  std::cout << "         1    1    2    2    3\n";
  std::cout << "1---5----0----5----0----5----0\n";

  pp << "int foo()\n"
        "{\n"
        ;
  pp.ind(+2);
  pp << "printf(\b\"hello there %d!\\n\",\r123456789\f);\n";
  pp << "bar(\b1 +\r2 +\r3 +\r4 +\r5 +\r6 +\r7 +\r8 +\r9 +\r10\f);\n";
  pp << "baz(\b\"a really long line that has no optional breaks at all\"\f);\n";
  pp << "zoo(\b\"one break is here, but it is very\",\r\"far from the start\"\f);\n";
  pp << "assert(\bx ==\ry &&\rz ==\rw &&\r"
               "(\bmoron !=\rfool ||\rtaxes->theRich\f)\f);\n";
  pp << "\aforall(x, y, z). if {\r"
          "x == yooey_more;\r"
          "yowza != fooey;\f\r"
        "} {\a\r"
          "z(x,y,z)==3;\r"
          "ay_caramba;\f\r"
        "}\n";
  pp.ind(-2);
  pp << "}\n";

  std::cout << pp.sb;

  return 0;
}


#endif // TEST_PPRINT
