// gramlex.h            see license.txt for copyright and terms of use
// GrammarLexer: a c++ lexer class for use with Flex's generated c++ scanner
// this lexer class is used both for parsing both AST and grammar descriptions;
// they differ in their .lex description, but their lexing state is the same

#ifndef __GRAMLEX_H
#define __GRAMLEX_H


// This included file is part of the Flex distribution.  It is
// installed in /usr/include on my Linux machine.  By including it, we
// get the declaration of the yyFlexLexer class.  Note that the file
// that flex generates, gramlex.yy.cc, also #includes this file.
// Perhaps also worth mentioning: I'm developing this with flex 2.5.4.
//
// update: This approach was too problematic.  I've taken to distributing
// FlexLexer.h myself.
#include "sm_flexlexer.h"

#include <iostream>         // istream

// token code definitions
#define TOK_EOF 0             // better name
#define TOK_INCLUDE 1         // not seen by parser


// other includes
#include "sm_str.h"
#include "sm_objlist.h"
#include "sm_srcloc.h"
#include "ast_embedded.h"
#include "ast_strtable.h"


// this class just holds the lexer state so it is properly encapsulated
// (and therefore, among other things, re-entrant)
class GrammarLexer : public yyFlexLexer, public ReportError {
public:      // types
  enum Constants {
    lexBufferSize = 4096,          // size of new lex buffers
  };

  // return true if the given token code is one of those representing
  // embedded text
  typedef bool (*isEmbedTok)(int tokCode);

  // error reporter that uses fileState instead of tokenStartLoc
  class AltReportError : public ReportError {
    GrammarLexer &lexer;

  public:
    AltReportError(GrammarLexer &L) : lexer(L) {}

    virtual void reportError(char const *msg);
    virtual void reportWarning(char const *msg);
  };
  friend class AltReportError;

public:      // data
  // exposed so a user-provided 'embedded' can use it
  AltReportError altReporter;

private:     // data
  // state of a file we were or are lexing
  struct FileState {
    SourceLoc loc;                 // location in the file
    std::istream *source;               // (owner?) source stream
    yy_buffer_state *bufstate;     // (owner?) flex's internal buffer state

  public:
    FileState(char const *filename, std::istream *source);
    ~FileState();

    FileState(FileState const &obj);
    FileState& operator= (FileState const &obj);
  };

  FileState fileState;             // state for file we're lexing now
  ObjList<FileState> fileStack;    // stack of files we will return to

  SourceLoc tokenStartLoc;         // location of start of current token

  // support for embedded code
  char embedStart;                 // if nonzero, punctuation that triggers
                                   // embedded processing
  char embedFinish;                // which character ends the embedded section
  int embedMode;                   // TOK_FUNDECL_BODY or TOK_FUN_BODY
  EmbeddedLang *embedded;          // (owner) the processor
  isEmbedTok embedTokTest;         // for printing diagnostics
  bool allowInit;                  // true if embedded can have an initializer

  int prevState;                   // so /**/ doesn't change start state

  int prevToken;                   // last token code yielded (ugly hack)

public:      // data
  // todo: can eliminate commentStartLine in favor of tokenStartLoc?
  //int commentStartLine;            // for reporting unterminated C comments
  int integerLiteral;              // to store number literal value
  StringRef sm_stringLiteral;         // sm_string in quotes, minus the quotes
  StringRef includeFileName;       // name in an #include directive

  // defined in the base class, FlexLexer:
  //   const char *YYText();           // start of matched text
  //   int YYLeng();                   // number of matched characters

  StringTable &strtable;           // sm_string table

  // count of errors encountered
  int errors;

private:     // funcs
  // disallowed
  GrammarLexer(GrammarLexer const &);

  // called to advance the column count
  void advCol(int n)
    { fileState.loc = sourceLocManager->advCol(fileState.loc, n); }

  // called when a newline is encountered
  void newLine()
    { fileState.loc = sourceLocManager->advLine(fileState.loc); }

  // adds a sm_string with only the specified # of chars; writes (but
  // then restores) a null terminator if necessary, so 'str' isn't const
  StringRef addString(char *str, int len) const;

  // nominally true if 'ch' equals 'embedFinish', but with a niggle
  bool embedFinishMatches(char ch) const;

public:      // funcs
  // create a new lexer that will read from to named stream,
  // or stdin if it is NULL
  GrammarLexer(isEmbedTok embedTokTest,
               StringTable &strtable,
               char const *fname = "<stdin>",
               std::istream * /*owner*/ source = NULL,
               EmbeddedLang * /*owner*/ embedded = NULL /*i.e. assume C lexics*/);

  // clean up
  ~GrammarLexer();

  // get current token as a sm_string
  StringRef curToken() const;
  int curLen() const { return const_cast<GrammarLexer*>(this)->YYLeng(); }

  // current token's embedded text
  StringRef curFuncBody() const;
  StringRef curDeclBody() const { return curFuncBody(); }    // implementation artifact
  StringRef curDeclName() const;

  // read the next token and return its code; returns TOK_EOF for end of file;
  // this function is defined in flex's output source code; this one
  // *does* return TOK_INCLUDE
  virtual int yylex();

  // similar to yylex, but process TOK_INCLUDE internally
  int yylexInc();

  // begin an embedded sequence
  void beginEmbed(char finish, int mode, int initNest = 0)
  {
    embedded->reset(initNest);
    embedFinish = finish;
    embedMode = mode;
  }

  // info about location of current token
  char const *curFname() const
    { return sourceLocManager->getFile(tokenStartLoc); }
  int curLine() const
    { return sourceLocManager->getLine(tokenStartLoc); }
  int curCol() const
    { return sourceLocManager->getCol(tokenStartLoc); }
  SourceLoc curLoc() const { return tokenStartLoc; }
  sm_string curLocStr() const;    // sm_string with file/line/col

  // error reporting; called by the lexer code
  void err(char const *msg) { reportError(msg); }     // msg should not include a newline
  void errorUnterminatedComment();
  void errorMalformedInclude();
  void errorIllegalCharacter(char ch);

  void printError(SourceLoc loc, char const *msg);
  void printWarning(SourceLoc loc, char const *msg);

  // for processing includes
  void recursivelyProcess(char const *fname, std::istream * /*owner*/ source);
  void popRecursiveFile();
  bool hasPendingFiles() const;

  // ReportError funcs
  virtual void reportError(char const *msg);
  virtual void reportWarning(char const *msg);
};


#endif // __GRAMLEX_H
