// boxprint.h
// another pretty-printing module, this one based on the box model
// described at http://caml.inria.fr/FAQ/format-eng.html

#ifndef BOXPRINT_H
#define BOXPRINT_H

#include "sm_str.h"
#include "sm_astlist.h"
#include "sm_array.h"


// fwd
class BoxPrint;


// manages the process of rendering a boxprint tree to a sm_string
class BPRender {
public:
  // output sm_string
  sm_stringBuilder sb;

  // right margin column; defaults to 72
  int margin;

  // column for next output; equal to the number of characters
  // after the last newline in 'sb'
  int curCol;

  // text to begin every line with; not counted towards column
  // counts; defaults to ""
  sm_string lineStartText;

public:
  BPRender();
  ~BPRender();

  // chars in the current line
  int getCurCol() const { return curCol; }

  // chars remaining on current line before the margin; might
  // be negative if the input didn't have enough breaks
  int remainder() const { return margin - getCurCol(); }

  // add some text (that doesn't include newlines) to the output
  void add(char const *text);

  // add a newline, plus indentation to get to column 'ind'
  void breakLine(int ind);

  // take the sm_string out of the rendering engine, replacing it
  // with the empty sm_string
  sm_string takeString() {
    sm_string ret(sb);
    reset();
    return ret;
  }

  // just clear the buffer to its original state; must do this
  // manually after changing 'lineStartText'
  void reset();

  // take the tree out of a boxprint builder, convert it to a sm_string,
  // and delete the tree
  sm_string takeAndRender(BoxPrint &bld);
};


// interface for elements in a boxprint tree
class BPElement {
public:
  // if no breaks are taken, compute the # of columns
  virtual int oneLineWidth()=0;

  // render this element as a sm_string with newlines, etc.
  virtual void render(BPRender &mgr)=0;

  // true if this element is a BPBreak and is enabled; returns false
  // by default
  virtual bool isBreak() const;

  // print the boxprint tree; for debugging code that produces them;
  // these methods do not emit leading or trailing whitespace
  virtual void debugPrint(std::ostream &os, int ind) const =0;

  // deallocate the element
  virtual ~BPElement();
};


// leaf in the tree: text to print
class BPText : public BPElement {
public:
  sm_string text;

public:
  BPText(char const *t);
  ~BPText();

  // BPElement funcs
  virtual int oneLineWidth();
  virtual void render(BPRender &mgr);
  virtual void debugPrint(std::ostream &os, int ind) const;
};


// leaf in the tree: a "break", which might end up being a
// space or a newline+indentation
class BPBreak : public BPElement {
public:
  // When true, this is a conditional break, and whether it is taken
  // or not depends on the prevailing break strategy of the box in
  // which it is located.  When false, the break is never taken, so
  // this is effectively just a space.
  bool enabled;

  // Nominally, when a break is taken, the indentation used is such
  // that the next char in the box is directly below the first char
  // in the box.  When this break is passed, however, it can add to
  // that nominal indent of 0; these adjustments accumulate as the
  // box is rendered.
  int indent;

public:
  BPBreak(bool e, int i);
  ~BPBreak();

  // BPElement funcs
  virtual int oneLineWidth();
  virtual void render(BPRender &mgr);
  virtual bool isBreak() const;
  virtual void debugPrint(std::ostream &os, int ind) const;
};


// kinds of boxes
enum BPKind {
  // enabled breaks are always taken
  BP_vertical,

  // enabled breaks are individually taken or not taken depending
  // on how much room is available; "hov"
  BP_sequence,

  // either all enabled breaks are taken, or none are taken; "h/v"
  BP_correlated,

  // # of kinds, also used to signal the end of a box in some cases
  NUM_BPKINDS
};

// internal node in the tree: a list of subtrees, some of which
// may be breaks
class BPBox : public BPElement {
public:
  // subtrees
  ASTList<BPElement> elts;

  // break strategy for this box
  BPKind kind;

public:
  BPBox(BPKind k);
  ~BPBox();

  // BPElement funcs
  virtual int oneLineWidth();
  virtual void render(BPRender &mgr);
  virtual void debugPrint(std::ostream &os, int ind) const;
};


// assists in the process of building a box tree by providing
// a number of syntactic shortcuts
class BoxPrint {
public:      // types
  // additional command besides BPKind
  enum Cmd {
    sp,          // insert disabled break
    br,          // insert enabled break
    ind,         // ibr(levelIndent)
    und,         // ibr(-levelIndent) ("unindent")
  };

  // insert enabled break with indentation
  struct IBreak {
    int indent;
    IBreak(int i) : indent(i) {}
    // use default copy ctor
  };

  // operator sequence
  struct Op {
    char const *text;
    Op(char const *t) : text(t) {}
    // default copy ctor
  };

private:     // data
  // stack of open boxes; always one open vert box at the top
  ObjArrayStack<BPBox> boxStack;

public:      // data
  // convenient names for the box kinds
  static BPKind const vert;       // = BP_vertical
  static BPKind const seq;        // = BP_sequence
  static BPKind const hv;         // = BP_correlated ("h/v")
  static BPKind const end;        // = NUM_BPKINDS

  // indentation amount for the ind/und commands; defaults to 2
  int levelIndent;

private:     // funcs
  // innermost box being built
  BPBox *box() { return boxStack.top(); }

public:      // funcs
  BoxPrint();
  ~BoxPrint();

  // append another element to the current innermost box
  void append(BPElement *elt);

  // add BPText nodes to current box
  BoxPrint& operator<< (int i);
  BoxPrint& operator<< (char const *s);

  // open/close boxes
  BoxPrint& operator<< (BPKind k);

  // insert breaks
  BoxPrint& operator<< (Cmd c);

  // insert break with indentation
  static IBreak ibr(int i) { return IBreak(i); }
  BoxPrint& operator<< (IBreak b);

  // op(text) is equivalent to sp << text << br
  static Op op(char const *text) { return Op(text); }
  BoxPrint &operator << (Op o);

  // take the accumulated box tree out; all opened boxes must have
  // been closed; the builder is left in a state where it can be used
  // to build a new tree if desired, or it can be simply destroyed
  BPBox* /*owner*/ takeTree();

  // print the current stack of trees
  void debugPrint(std::ostream &os) const;
  void debugPrintCout() const;      // for gdb
};


#endif // BOXPRINT_H
