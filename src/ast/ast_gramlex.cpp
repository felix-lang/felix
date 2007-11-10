// gramlex.cc            see license.txt for copyright and terms of use
// code for gramlex.h

#include "ast_gramlex.h"
#include "sm_trace.h"
#include "ast_ccsstr.h"
#include "sm_ckheap.h"

#include <fstream>     // std::cout, ifstream


// workaround for flex-2.5.31
#ifdef FLEX_STD    // detect later versions of flex
  // copied from flex's output
  #define YY_CURRENT_BUFFER ( (yy_buffer_stack) \
                            ? (yy_buffer_stack)[(yy_buffer_stack_top)] \
                            : NULL)

  // the 'yy_current_buffer' field was replaced by the buffer stack
  // alluded to above
  #define yy_current_buffer YY_CURRENT_BUFFER
#endif // FLEX_STD


// ----------------- GrammarLexer::AltReportError ---------------
void GrammarLexer::AltReportError::reportError(char const *msg)
{
  lexer.printError(lexer.fileState.loc, msg);
}

void GrammarLexer::AltReportError::reportWarning(char const *msg)
{
  lexer.printWarning(lexer.fileState.loc, msg);
}


// ----------------- GrammarLexer::FileState --------------------
GrammarLexer::FileState::FileState(char const *filename, std::istream *src)
  : loc(sourceLocManager->encodeBegin(filename)),
    source(src),
    bufstate(NULL)
{}


GrammarLexer::FileState::~FileState()
{
  // we let ~GrammarLexer take care of deletions here since we
  // have to know what ~yyFlexLexer is going to do, and we
  // don't have enough context here to know that
}


GrammarLexer::FileState::FileState(FileState const &obj)
{
  *this = obj;
}


GrammarLexer::FileState &GrammarLexer::FileState::
  operator= (FileState const &obj)
{
  if (this != &obj) {
    loc = obj.loc;
    source = obj.source;
    bufstate = obj.bufstate;
  }
  return *this;
}


// ---------------------- GrammarLexer --------------------------
GrammarLexer::GrammarLexer(isEmbedTok test, StringTable &strtbl,
                           char const *fname, std::istream *source,
                           EmbeddedLang *userEmb)
  : yyFlexLexer(source),
    altReporter(*this),
    fileState(fname, source),
    fileStack(),
    tokenStartLoc(SL_UNKNOWN),
    embedStart(0),
    embedFinish(0),
    embedMode(0),
    embedded(userEmb? userEmb : new CCSubstrate(&altReporter)),
    embedTokTest(test),
    allowInit(false),
    prevState(0),       // same as INITIAL, but this value isn't used
    prevToken(0),       // hack..
    integerLiteral(0),
    sm_stringLiteral(""),
    includeFileName(""),
    strtable(strtbl),
    errors(0)
{
  trace("tmp") << "source is " << source << std::endl;

  // grab initial buffer object so we can restore it after
  // processing an include file (turns out this doesn't work
  // because it's NULL now; see recursivelyProcess())
  fileState.bufstate = yy_current_buffer;
}

GrammarLexer::~GrammarLexer()
{
  // ~yyFlexLexer deletes its current buffer, but not any
  // of the istream sources it's been passed

  // first let's unpop any unpopped input files
  while (hasPendingFiles()) {
    popRecursiveFile();
  }

  // now delete the original istream source
  //
  // 10/09/04: This used to say "fileState.source != std::cin", but that
  // invokes std::cin.operator void*(), which always returns 0 or -1 in
  // gcc-2.95.3's library.  I believe I intended to compare addresses,
  // though at this point I'm not sure since I don't know where the
  // call sites to the constructor are.  (I found this problem because
  // at one point Elsa (erroneously) choked on this construction.)
  if (fileState.source &&
      fileState.source != &std::cin) {
    //checkHeap();
    //checkHeapNode(fileState.source);   // this is wrong b/c of virtual inheritance..
    delete fileState.source;
    //checkHeap();
  }

  delete embedded;
}


int GrammarLexer::yylexInc()
{
  // get raw token
  int code = yylex();

  // save this code for next time; part of what makes this hack
  // problematic is that this assignment is only performed if the
  // client calls 'yylexInc'..
  prevToken = code;

  // include processing
  if (code == TOK_INCLUDE) {
    sm_string fname = includeFileName;

    // 'in' will be deleted in ~GrammarLexer
    std::ifstream *in = new std::ifstream(fname);
    if (!*in) {
      err(sm_stringc << "unable to open include file `" << fname << "'");
    }
    else {
      recursivelyProcess(fname, in);
    }

    // go to next token (tail recursive)
    return yylexInc();
  }

  if (code == TOK_EOF  &&  hasPendingFiles()) {
    popRecursiveFile();
    return yylexInc();
  }

  #if 1
  // possible performance problem
  if (embedTokTest(code)) {
    trace("lex") << "yielding embedded (" << code << ") at "
                 << curLocStr() << ": "
                 << curFuncBody() << std::endl;
  }
  else {
    trace("lex") << "yielding token (" << code << ") "
                 << curToken() << " at "
                 << curLocStr() << std::endl;
  }
  #endif // 0/1

  // nothing special
  return code;
}


StringRef GrammarLexer::curToken() const
{
  return addString(yytext, yyleng);
}

StringRef GrammarLexer::addString(char *str, int len) const
{
  // write a null terminator temporarily
  char wasThere = str[len];
  if (wasThere) {
    str[len] = 0;
    StringRef ret = strtable.add(str);
    str[len] = wasThere;
    return ret;
  }
  else {
    return strtable.add(str);
  }
}


bool GrammarLexer::embedFinishMatches(char ch) const
{
  return ch == embedFinish ||
         (allowInit && ch=='=');     // to handle initial value syntax
}


StringRef GrammarLexer::curFuncBody() const
{
  return strtable.add(embedded->getFuncBody());
}


StringRef GrammarLexer::curDeclName() const
{
  return strtable.add(embedded->getDeclName());
}


sm_string GrammarLexer::curLocStr() const
{
  return toString(curLoc());
}


void GrammarLexer::reportError(char const *msg)
{
  printError(curLoc(), msg);
}

void GrammarLexer::printError(SourceLoc loc, char const *msg)
{
  errors++;
  std::cerr << toString(loc) << ": error: " << msg << std::endl;
}


void GrammarLexer::reportWarning(char const *msg)
{
  printWarning(curLoc(), msg);
}

void GrammarLexer::printWarning(SourceLoc loc, char const *msg)
{
  std::cerr << toString(loc) << ": warning: " << msg << std::endl;
}


void GrammarLexer::errorUnterminatedComment()
{
  err(sm_stringc << "unterminated comment, beginning on line " //<< commentStartLine);
              << sourceLocManager->getLine(tokenStartLoc));
}

void GrammarLexer::errorMalformedInclude()
{
  err(sm_stringc << "malformed include");
}

void GrammarLexer::errorIllegalCharacter(char ch)
{
  err(sm_stringc << "illegal character: `" << ch << "'");
}


void GrammarLexer::recursivelyProcess(char const *fname, std::istream *source)
{
  trace("lex") << "recursively processing " << fname << std::endl;

  // grab current buffer; this is necessary because when we
  // tried to grab it in the ctor it was NULL
  fileState.bufstate = yy_current_buffer;
  xassert(fileState.bufstate);

  // push current state
  fileStack.prepend(new FileState(fileState));

  // reset current state
  fileState = FileState(fname, source);

  // storing this in 'bufstate' is redundant because of the
  // assignment above, but no big deal
  fileState.bufstate = yy_create_buffer(source, lexBufferSize);

  // switch underlying lexer over to new file
  yy_switch_to_buffer(fileState.bufstate);
}


void GrammarLexer::popRecursiveFile()
{
  trace("lex") << "done processing " <<
    sourceLocManager->getFile(fileState.loc) << std::endl;

  // among other things, this prevents us from accidentally deleting
  // flex's first buffer (which it presumably takes care of) or
  // deleting 'std::cin'
  xassert(hasPendingFiles());

  // close down stuff associated with current file
  yy_delete_buffer(fileState.bufstate);
  delete fileState.source;

  // pop stack
  FileState *st = fileStack.removeAt(0);
  fileState = *st;
  delete st;

  // point flex at the new (old) buffer
  yy_switch_to_buffer(fileState.bufstate);
}


bool GrammarLexer::hasPendingFiles() const
{
  return fileStack.isNotEmpty();
}



#ifdef TEST_GRAMLEX

// defined in gramlex.lex
bool isGramlexEmbed(int code);

int main(int argc)
{
  SourceLocManager mgr;
  GrammarLexer lexer(isGramlexEmbed);
  traceAddSys("lex");

  std::cout << "go!\n";

  while (1) {
    // any argument disables include processing
    int code = argc==1? lexer.yylexInc() : lexer.yylex();
    if (code == 0) {  // eof
      break;
    }

    else if (isGramlexEmbed(code)) {
      std::cout << "embedded code at " << lexer.curLocStr()
           << ": " << lexer.curFuncBody()
           << std::endl;
    }

    else if (code == TOK_INCLUDE) {
      // if I use yylexInc above, this is never reached
      std::cout << "include at " << lexer.curLocStr()
           << ": filename is `" << lexer.includeFileName.pcharc()
           << "'\n";
    }

    else {
      std::cout << "token at " << lexer.curLocStr()
           << ": code=" << code
           << ", text: " << lexer.curToken().pcharc()
           << std::endl;
    }
  }

  return 0;
}

#endif // TEST_GRAMLEX
