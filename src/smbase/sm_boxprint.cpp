// boxprint.cc
// code for boxprint.h

#include "sm_boxprint.h"
#include "sm_strutil.h"

#include <cstring>         // std::strlen


// ----------------------- BPRender ----------------------
BPRender::BPRender()
  : sb(),         // initially empty
    margin(72),
    curCol(0),
    lineStartText("")
{}

BPRender::~BPRender()
{}


void BPRender::reset()
{
  sb.clear();
  sb << lineStartText;
}


void BPRender::add(char const *text)
{
  int len = std::strlen(text);
  sb << text;
  curCol += len;
}

void BPRender::breakLine(int ind)
{
  sb << "\n" << lineStartText;

  for (int i=0; i < ind; i++) {
    sb << ' ';
  }

  curCol = ind;
}


sm_string BPRender::takeAndRender(BoxPrint &bld)
{
  BPBox* /*owner*/ tree = bld.takeTree();
  tree->render(*this);
  sm_string ret(sb);
  sb.clear();
  delete tree;
  return ret;
}


// ----------------------- BPElement ---------------------
bool BPElement::isBreak() const
{
  return false;
}

BPElement::~BPElement()
{}


// ------------------------- BPText ----------------------
BPText::BPText(char const *t)
  : text(t)
{}

BPText::~BPText()
{}


int BPText::oneLineWidth()
{
  return text.length();
}

void BPText::render(BPRender &mgr)
{
  mgr.add(text);
}


void BPText::debugPrint(std::ostream &os, int /*ind*/) const
{
  os << "text(" << quoted(text) << ")";
}


// ------------------------ BPBreak ---------------------
BPBreak::BPBreak(bool e, int i)
  : enabled(e),
    indent(i)
{}

BPBreak::~BPBreak()
{}

int BPBreak::oneLineWidth()
{
  return 1;
}

void BPBreak::render(BPRender &mgr)
{
  // if we're being asked to render, then this break must not be taken
  mgr.add(" ");
}

bool BPBreak::isBreak() const
{
  return enabled;
}

void BPBreak::debugPrint(std::ostream &os, int /*ind*/) const
{
  os << "break(en=" << (int)enabled << ", ind=" << indent << ")";
}


// ------------------------- BPBox ------------------------
BPBox::BPBox(BPKind k)
  : elts(),      // initially empty
    kind(k)
{
  xassert((unsigned)k < NUM_BPKINDS);
}

BPBox::~BPBox()
{}


int BPBox::oneLineWidth()
{
  int sum = 0;
  FOREACH_ASTLIST_NC(BPElement, elts, iter) {
    sum += iter.data()->oneLineWidth();
  }
  return sum;
}


// this function is the heart of the rendering engine
void BPBox::render(BPRender &mgr)
{
  int startCol = mgr.getCurCol();

  if (kind == BP_vertical ||
      (kind == BP_correlated && oneLineWidth() > mgr.remainder())) {
    // take all of the breaks
    FOREACH_ASTLIST_NC(BPElement, elts, iter) {
      BPElement *elt = iter.data();
      if (elt->isBreak()) {
        startCol += static_cast<BPBreak*>(elt)->indent;
        mgr.breakLine(startCol);
      }
      else {
        elt->render(mgr);
      }
    }
    return;
  }

  if (kind == BP_correlated) {
    // if we got here, we're taking none of the breaks
    FOREACH_ASTLIST_NC(BPElement, elts, iter) {
      BPElement *elt = iter.data();
      elt->render(mgr);
    }
    return;
  }

  xassert(kind == BP_sequence);

  // this cursor points to the next element that has not been rendered
  ASTListIterNC<BPElement> cursor(elts);

  // when not NULL, the cursor has just passed a break, but we haven't
  // actually decided whether to take it or not
  BPBreak *pendingBreak = NULL;

  while (!cursor.isDone()) {
    // is there room for the elements up to the first break?
    int segmentWidth = pendingBreak? 1 : 0;
    ASTListIterNC<BPElement> lookahead(cursor);
    while (!lookahead.isDone() && !lookahead.data()->isBreak()) {
      segmentWidth += lookahead.data()->oneLineWidth();
      lookahead.adv();
    }

    if (pendingBreak && segmentWidth > mgr.remainder()) {
      // take the pending break
      startCol += pendingBreak->indent;
      mgr.breakLine(startCol);
      pendingBreak = NULL;
    }

    // the segment will be put here without a preceding break
    else if (pendingBreak) {
      pendingBreak->render(mgr);
      pendingBreak = NULL;
    }

    xassert(pendingBreak == NULL);

    // render the segment
    while (!cursor.isDone() && !cursor.data()->isBreak()) {
      cursor.data()->render(mgr);
      cursor.adv();
    }

    if (!cursor.isDone()) {
      // we stopped on a break
      pendingBreak = static_cast<BPBreak*>(cursor.data());
      cursor.adv();
    }
  }

  if (pendingBreak) {
    // ended with a break.. strange, but harmless I suppose
    pendingBreak->render(mgr);
  }
}


void BPBox::debugPrint(std::ostream &os, int ind) const
{
  static char const * const map[] = {
    "vert",
    "seq",
    "corr"
  };

  os << "box(kind=" << map[kind] << ") {\n";
  ind += 2;

  FOREACH_ASTLIST(BPElement, elts, iter) {
    for (int i=0; i<ind; i++) {
      os << " ";
    }

    iter.data()->debugPrint(os, ind);
    os << "\n";
  }

  ind -= 2;
  for (int i=0; i<ind; i++) {
    os << " ";
  }
  os << "}";
}


// ------------------------ BoxPrint ----------------------
BPKind const BoxPrint::vert = BP_vertical;
BPKind const BoxPrint::seq  = BP_sequence;
BPKind const BoxPrint::hv   = BP_correlated;
BPKind const BoxPrint::end  = NUM_BPKINDS;


BoxPrint::BoxPrint()
  : boxStack(),
    levelIndent(2)
{
  // initial vert box
  boxStack.push(new BPBox(BP_vertical));
}

BoxPrint::~BoxPrint()
{}


void BoxPrint::append(BPElement *elt)
{
  box()->elts.append(elt);
}


BoxPrint& BoxPrint::operator<< (int i)
{
  return operator<< (sm_stringc << i);
}

BoxPrint& BoxPrint::operator<< (char const *s)
{
  append(new BPText(s));
  return *this;
}


BoxPrint& BoxPrint::operator<< (BPKind k)
{
  if (k == NUM_BPKINDS) {
    // close current box
    append(boxStack.pop());
  }
  else {
    // open new box
    boxStack.push(new BPBox(k));
  }
  return *this;
}


BoxPrint& BoxPrint::operator<< (Cmd c)
{
  if (c == br || c == sp) {
    append(new BPBreak(c==br /*enabled*/, 0 /*indent*/));
  }
  else {
    append(new BPBreak(true /*enabled*/, c==ind? levelIndent : -levelIndent));
  }
  return *this;
}


BoxPrint& BoxPrint::operator<< (IBreak b)
{
  append(new BPBreak(true /*enabled*/, b.indent /*indent*/));
  return *this;
}


BoxPrint& BoxPrint::operator<< (Op o)
{
  return *this << sp << o.text << br;
}


BPBox* /*owner*/ BoxPrint::takeTree()
{
  // all boxes must be closed
  xassert(boxStack.length() == 1);

  BPBox *ret = boxStack.pop();

  // initialize the box stack again, in case the user wants
  // to build another tree
  boxStack.push(new BPBox(BP_vertical));

  return ret;
}


void BoxPrint::debugPrint(std::ostream &os) const
{
  for (int i=0; i < boxStack.length(); i++) {
    os << "----- frame -----\n";
    boxStack[i]->debugPrint(os, 0 /*ind*/);
    os << "\n";
  }
}

void BoxPrint::debugPrintCout() const
{
  debugPrint(std::cout);
}


// ------------------------ test code ----------------------
#ifdef TEST_BOXPRINT

#include <stdlib.h>       // atoi
#include "sm_ckheap.h"

void doit(int argc, char *argv[])
{
  BoxPrint bp;

  bp << "int foo()" << bp.br
     << "{" << bp.ind;

  bp << "printf(" << bp.seq
        << "\"hello there %d!\\n\"," << bp.br
        << "123456789"
     << bp.end << ");" << bp.br;

  bp << "bar(" << bp.seq
        << "1" << bp.op("+")
        << "2" << bp.op("+")
        << "3" << bp.op("+")
        << "4" << bp.op("+")
        << "5" << bp.op("+")
        << "6" << bp.op("+")
        << "7" << bp.op("+")
        << "8" << bp.op("+")
        << "9" << bp.op("+")
        << "10"
     << bp.end << ");" << bp.br;

  bp << "baz(" << bp.seq
        << "\"a really long line that has no optional breaks at all\""
     << bp.end << ");" << bp.br;

  bp << "zoo(" << bp.seq
        << "\"one break is here, but it is very\"," << bp.br
        << "\"far from the start\""
     << bp.end << ");" << bp.br;

  bp << "assert(" << bp.seq
        << bp.seq << "x" << bp.op("=") << "y" << bp.end << bp.op("&&")
        << bp.seq << "z" << bp.op("=") << "w" << bp.end << bp.op("&&")
        << "(" << bp.seq
           << bp.seq << "moron" << bp.op("!=") << "fool" << bp.end << bp.op("||")
           << "taxes->theRich"
        << bp.end << ")"
     << bp.end << ")" << bp.br;

  bp << bp.hv
        << "forall(" << bp.seq
           << "x," << bp.br << "y," << bp.br << "z"
        << bp.end << "). if {" << bp.ind
        << bp.seq << "x" << bp.op("==") << "yooey_more" << bp.end << ";" << bp.br
        << bp.seq << "yowza" << bp.op("!=") << "fooey" << bp.end << ";"
        << bp.und << "} /*==>*/ {" << bp.ind
        << bp.seq << "z(x,y,z)" << bp.op("==") << "3" << bp.end << ";" << bp.br
        << "ay_caramba" << ";"
        << bp.und << "};"
     << bp.end;

  bp << bp.und << "}" << bp.br;

  BPBox *tree = bp.takeTree();

  BPRender ren;
  ren.margin = 30;
  if (argc >= 2) {
    ren.margin = atoi(argv[1]);
  }
  std::cout << "margin: " << ren.margin << "\n";

  tree->render(ren);
  delete tree;

  std::cout << "         1    1    2    2    3    3    4    4    5    5    6    6    7\n";
  std::cout << "1---5----0----5----0----5----0----5----0----5----0----5----0----5----0\n";
  std::cout << ren.takeString();
}

int main(int argc, char *argv[])
{
  doit(argc, argv);
  //malloc_stats();
  return 0;
}

#endif // TEST_BOXPRINT

