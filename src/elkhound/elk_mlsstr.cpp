// mlsstr.cc            see license.txt for copyright and terms of use
// code for mlsstr.h
// based on ccsstr.cc

#include "elk_mlsstr.h"
#include "sm_xassert.h"
#include "sm_exc.h"
#include "sm_strutil.h"

#include <iostream>    // std::cout
#include <ctype.h>       // isspace


MLSubstrate::MLSubstrate(ReportError *err)
  : EmbeddedLang(err)
{
  reset();
}

void MLSubstrate::reset(int initNest)
{
  state = ST_NORMAL;
  nesting = initNest;
  comNesting = 0;
  prev = 0;
  text.setlength(0);
}


MLSubstrate::~MLSubstrate()
{}


void MLSubstrate::handle(char const *str, int len, char finalDelim)
{
  text.append(str, len);

  for (; len>0; len--,str++) {
    switch (state) {
      case ST_NORMAL:
        switch (*str) {
          case '{':
          case '(':
          case '[':
            nesting++;
            break;

          case '}':
          case ')':
          case ']':
            if (nesting == 0) {
              err->reportError(sm_stringc
                << "unexpected closing delimiter `" << *str
                << "' -- probably due to missing `" << finalDelim << "'");
            }
            else {
              nesting--;
            }
            break;

          case '\"':
            state = ST_STRING;
            break;

          case '\'':
            state = ST_CHAR;
            break;

          case '*':
            if (prev == '(') {
              state = ST_COMMENT;
              xassert(comNesting == 0);
              xassert(nesting > 0);
              nesting--;     // undo 'nesting++' from the '('

              // if the next char is ')', i.e. input was "(*)", do
              // not allow it to use this '*' to finish the comment
              prev = 0;
              continue;
            }
            break;
        }
        break;

      case ST_STRING:
      case ST_CHAR:
        if (prev != '\\') {
          if ((state == ST_STRING && *str == '\"') ||
              (state == ST_CHAR && *str == '\'')) {
            state = ST_NORMAL;
          }
          else if (*str == '\n') {
            err->reportError("unterminated sm_string or char literal");
          }
        }
        break;

      case ST_COMMENT:
        if (prev == '(' && *str == '*') {
          comNesting++;
          prev = 0;      // like above
          continue;
        }
        else if (prev == '*' && *str == ')') {
          xassert(comNesting >= 0);
          if (comNesting == 0) {
            // done with comment
            state = ST_NORMAL;
          }
          else {
            // decrease nesting
            comNesting--;
          }
        }
        break;

      default:
        xfailure("unknown state");
    }

    prev = *str;
  }
}


bool MLSubstrate::zeroNesting() const
{
  return state == ST_NORMAL && nesting == 0;
}


sm_string MLSubstrate::getFuncBody() const
{
  return text;
}


// 4/29/04: I have no idea if this is right or not.. this is the
// definition from ccsstr.cc.
sm_string MLSubstrate::getDeclName() const
{
  // go with the rather inelegant heuristic that the word
  // just before the first '(' is the function's name
  char const *start = text.pcharc();
  char const *p = start;

  // find first '('
  while (*p && *p!='(') { p++; }
  if (!*p) {
    xformat("missing '('");
  }
  if (p == start) {
    xformat("missing name");
  }

  // skip backward past any whitespace before the '('
  p--;
  while (p>=start && isspace(*p)) { p--; }
  if (p<start) {
    xformat("missing name");
  }
  char const *nameEnd = p+1;    // char just past last

  // move backward through the name
  while (p>=start &&
         (isalnum(*p) || *p=='_'))
    { p--; }
  p++;    // move back to most recent legal char

  // done
  return sm_string(p, nameEnd-p);
}


// ------------------ test code -------------------
#ifdef TEST_MLSSTR

#define ML MLSubstrate
#define Test MLSubstrateTest

// test code is put into a class just so that MLSubstrate
// can grant it access to private fields
class Test {
public:
  void feed(ML &ml, char const *src);
  void test(char const *src, ML::State state, int nesting,
            int comNesting, char prev);
  void normal(char const *src, int nesting);
  void str(char const *src, int nesting, bool bs);
  void yes(char const *src);
  void no(char const *src);
  void name(char const *body, char const *n);
  void badname(char const *body);
  int main();
};


#define min(a,b) ((a)<(b)?(a):(b))

void Test::feed(ML &ml, char const *src)
{
  std::cout << "trying: " << src << std::endl;
  while (*src) {
    // feed it in 10 char increments, to test split processing too
    int len = min(strlen(src), 10);
    ml.handle(src, len, '}');
    src += len;
  }
}


void Test::test(char const *src, ML::State state, int nesting,
                int comNesting, char prev)
{
  ML ml;
  feed(ml, src);

  if (!( ml.state == state &&
         ml.nesting == nesting &&
         ml.prev == prev )) {
    xfailure(sm_stringc << "failed on src: " << src);
  }
}


void Test::normal(char const *src, int nesting)
{
  test(src, ML::ST_NORMAL, nesting, 0, src[strlen(src)-1]);
}

void Test::str(char const *src, int nesting, bool bs)
{
  char prev = (bs? '\\' : src[strlen(src)-1]);
  test(src, ML::ST_STRING, nesting, 0, prev);

  // repeat the test with single-tick
  sm_string another = replace(src, "\"", "\'");
  test(another, ML::ST_CHAR, nesting, 0, prev);
}


void Test::yes(char const *src)
{
  ML ml;
  feed(ml, src);

  xassert(ml.zeroNesting());
}

void Test::no(char const *src)
{
  ML ml;
  feed(ml, src);

  xassert(!ml.zeroNesting());
}

void Test::name(char const *body, char const *n)
{
  ML ml;
  feed(ml, body);
  xassert(ml.getDeclName().equals(n));
}

void Test::badname(char const *body)
{
  ML ml;
  feed(ml, body);
  try {
    ml.getDeclName();
    xfailure("got a name when it shoudn't have!");
  }
  catch (...)
    {}
}


int Test::main()
{
  normal("int main()", 0);
  normal("int main() { hi", 1);
  normal("int main() { hi {", 2);
  normal("int main() { hi { foo[5", 3);
  normal("int main() { hi { foo[5] and ", 2);
  normal("int main() { hi { foo[5] and } bar ", 1);
  normal("int main() { hi { foo[5] and } bar } baz ", 0);

  normal("main() { printf(\"hello \\ world\"); ret", 1);

  normal("()[]{}([{}])", 0);
  normal("{ ()[]{}([{}]) } ", 0);
  normal("( ()[]{}([{}]) )", 0);
  normal("[ ()[]{}([{}]) ]", 0);
  normal("\"foo\" ()[]{}([{}])", 0);

  str("main() { printf(\"hello", 2, false);
  str("main() { printf(\"hello \\", 2, true);
  str("main() { printf(\"hello \\ world", 2, false);
  str("main() { printf(\"hello \\ world\", \"hi", 2, false);

  test("\"a\" 'b' (", ML::ST_NORMAL, 1, 0, '(');

  // test comments, particularly testing
  test("(", ML::ST_NORMAL, 1, 0, '(');
  test("(*", ML::ST_COMMENT, 0, 0, 0);
  test("(*)", ML::ST_COMMENT, 0, 0, ')');
  test("(*)(", ML::ST_COMMENT, 0, 0, '(');
  test("(*)(*", ML::ST_COMMENT, 0, 1, 0);
  test("(*)(*)", ML::ST_COMMENT, 0, 1, ')');
  test("(*)(*)*", ML::ST_COMMENT, 0, 1, '*');
  test("(*)(*)*)", ML::ST_COMMENT, 0, 0, ')');
  test("(*)(*)*)*", ML::ST_COMMENT, 0, 0, '*');
  test("(*)(*)*)*)", ML::ST_NORMAL, 0, 0, ')');

  test("(*(*(*(*", ML::ST_COMMENT, 0, 4, 0);

  yes("main() {}");
  yes("main() { printf(\"foo\", 3, 4 (*yep{*)); }");
  yes("some (* junk {\n more*)");
  yes("'\\''");
  yes("\"\\\"\"");
  yes("[][][][][]");
  yes("\"[[[\"");
  yes("*");
  yes("(* [ / * [ *)");

  no("\"");
  no("(");
  no(" ( (* ) *) ");

  name("int main()", "main");
  name("int eval(Environment &env)", "eval");
  name("man()", "man");
  badname("(");
  badname("  (");
  badname("  ");
  badname("");
  badname(")");
  badname("main");

  std::cout << "\nmlsstr: all tests PASSED\n";

  return 0;
}

int main()
{
  Test t;
  return t.main();
}

#endif // TEST_MLSSTR
