// strutil.cc            see license.txt for copyright and terms of use
// code for strutil.h

#include "sm_strutil.h"
#include "sm_exc.h"
#include "sm_autofile.h"

#include <ctype.h>       // isspace
#include <cstring>      // strstr, std::memcmp
#include <stdio.h>       // sprintf
#include <stdlib.h>      // strtoul
#include <time.h>        // time, asctime, localtime


// replace all instances of oldstr in src with newstr, return result
sm_string replace(char const *src, char const *oldstr, char const *newstr)
{
  sm_stringBuilder ret("");

  while (*src) {
    char const *next = strstr(src, oldstr);
    if (!next) {
      ret &= sm_string(src);
      break;
    }

    // make a subsm_string out of the characters between src and next
    sm_string upto(src, next-src);

    // add it to running sm_string
    ret &= upto;

    // add the replace-with sm_string
    ret &= sm_string(newstr);

    // move src to beyond replaced subsm_string
    src += (next-src) + std::strlen(oldstr);
  }

  return ret;
}


sm_string expandRanges(char const *chars)
{
  sm_stringBuilder ret;

  while (*chars) {
    if (chars[1] == '-' && chars[2] != 0) {
      // range specification
      if (chars[0] > chars[2]) {
        xformat("range specification with wrong collation order");
      }

      for (char c = chars[0]; c <= chars[2]; c++) {
        ret << c;
      }
      chars += 3;
    }
    else {
      // simple character specification
      ret << chars[0];
      chars++;
    }
  }

  return ret;
}


sm_string translate(char const *src, char const *srcchars, char const *destchars)
{
  // first, expand range notation in the specification sequences
  sm_string srcSpec = expandRanges(srcchars);
  sm_string destSpec = expandRanges(destchars);

  // build a translation map
  char map[256];
  int i;
  for (i=0; i<256; i++) {
    map[i] = i;
  }

  // excess characters from either sm_string are ignored ("SysV" behavior)
  for (i=0; i < srcSpec.length() && i < destSpec.length(); i++) {
    map[(unsigned char)( srcSpec[i] )] = destSpec[i];
  }

  // run through 'src', applying 'map'
  sm_string ret(std::strlen(src));
  char *dest = ret.pchar();
  while (*src) {
    *dest = map[(unsigned char)*src];
    dest++;
    src++;
  }
  *dest = 0;    // final nul terminator

  return ret;
}


// why is this necessary?
sm_string sm_stringToupper(char const *src)
  { return translate(src, "a-z", "A-Z"); }


sm_string trimWhitespace(char const *str)
{
  // trim leading whitespace
  while (isspace(*str)) {
    str++;
  }

  // trim trailing whitespace
  char const *end = str + std::strlen(str);
  while (end > str &&
         isspace(end[-1])) {
    end--;
  }

  // return it
  return sm_string(str, end-str);
}


// table of escape codes
static struct Escape {
  char actual;      // actual character in sm_string
  char escape;      // char that follows backslash to produce 'actual'
} const escapes[] = {
  { '\0', '0' },  // nul
  { '\a', 'a' },  // bell
  { '\b', 'b' },  // backspace
  { '\f', 'f' },  // form feed
  { '\n', 'n' },  // newline
  { '\r', 'r' },  // carriage return
  { '\t', 't' },  // tab
  { '\v', 'v' },  // vertical tab
  { '\\', '\\'},  // backslash
  { '"',  '"' },  // double-quote
  { '\'', '\''},  // single-quote
};


sm_string encodeWithEscapes(char const *p, int len)
{
  sm_stringBuilder sb;

  for (; len>0; len--, p++) {
    // look for an escape code
    unsigned i;
    for (i=0; i<TABLESIZE(escapes); i++) {
      if (escapes[i].actual == *p) {
        sb << '\\' << escapes[i].escape;
        break;
      }
    }
    if (i<TABLESIZE(escapes)) {
      continue;   // found it and printed it
    }

    // try itself
    if (isprint(*p)) {
      sb << *p;
      continue;
    }

    // use the most general notation
    char tmp[5];
    sprintf(tmp, "\\x%02X", (unsigned char)(*p));
    sb << tmp;
  }

  return sb;
}


sm_string encodeWithEscapes(char const *p)
{
  return encodeWithEscapes(p, std::strlen(p));
}


sm_string quoted(char const *src)
{
  return sm_stringc << "\""
                 << encodeWithEscapes(src, std::strlen(src))
                 << "\"";
}


void decodeEscapes(sm_string &dest, int &destLen, char const *src,
                   char delim, bool allowNewlines)
{
  // place to collect the sm_string characters
  sm_stringBuilder sb;
  destLen = 0;

  while (*src != '\0') {
    if (*src == '\n' && !allowNewlines) {
      xformat("unescaped newline (unterminated sm_string)");
    }
    if (*src == delim) {
      xformat(sm_stringc << "unescaped delimiter (" << delim << ")");
    }

    if (*src != '\\') {
      // easy case
      sb << *src;
      destLen++;
      src++;
      continue;
    }

    // advance past backslash
    src++;

    // see if it's a simple one-char backslash code;
    // start at 1 so we do *not* use the '\0' code since
    // that's actually a special case of \0123', and
    // interferes with the latter
    int i;
    for (i=1; i<TABLESIZE(escapes); i++) {
      if (escapes[i].escape == *src) {
        sb << escapes[i].actual;
        destLen++;
        src++;
        break;
      }
    }
    if (i < TABLESIZE(escapes)) {
      continue;
    }

    if (*src == '\0') {
      xformat("backslash at end of sm_string");
    }

    if (*src == '\n') {
      // escaped newline; advance to first non-whitespace
      src++;
      while (*src==' ' || *src=='\t') {
        src++;
      }
      continue;
    }

    if (*src == 'x' || isdigit(*src)) {
      // hexadecimal or octal char (it's unclear to me exactly how to
      // parse these since it's supposedly legal to have e.g. "\x1234"
      // mean a one-char sm_string.. whatever)
      bool hex = (*src == 'x');
      if (hex) {
        src++;

        // strtoul is willing to skip leading whitespace
        if (isspace(*src)) {
          xformat("whitespace following hex (\\x) escape");
        }
      }

      char const *endptr;
      unsigned long val = strtoul(src, (char**)&endptr, hex? 16 : 8);
      if (src == endptr) {
        // this can't happen with the octal escapes because
        // there is always at least one valid digit
        xformat("invalid hex (\\x) escape");
      }

      sb << (unsigned char)val;    // possible truncation..
      destLen++;
      src = endptr;
      continue;
    }

    // everything not explicitly covered will be considered
    // an error (for now), even though the C++ spec says
    // it should be treated as if the backslash were not there
    //
    // 7/29/04: now making backslash the identity transformation in
    // this case
    //
    // copy character as if it had not been backslashed
    sb << *src;
    destLen++;
    src++;
  }

  // copy to 'dest'
  dest.setlength(destLen);       // this sets the NUL
  if (destLen > 0) {
    std::memcpy(dest.pchar(), sb.pchar(), destLen);
  }
}


sm_string parseQuotedString(char const *text)
{
  if (!( text[0] == '"' &&
         text[std::strlen(text)-1] == '"' )) {
    xformat(sm_stringc << "quoted sm_string is missing quotes: " << text);
  }

  // strip the quotes
  sm_string noQuotes = sm_string(text+1, std::strlen(text)-2);

  // decode escapes
  sm_string ret;
  int dummyLen;
  decodeEscapes(ret, dummyLen, noQuotes, '"');
  return ret;
}


sm_string localTimeString()
{
  time_t t = time(NULL);
  char const *p = asctime(localtime(&t));
  return sm_string(p, std::strlen(p) - 1);     // strip final newline
}

// rewritten by JMS to handle /, \ and : as separators
sm_string sm_basename(char const *src)
{
  int n = std::strlen(src);
  while(n>0 && (src[n-1] == '/' || src[n-1] == '\\' || src[n-1] == ':'))--n;
  int m = n;
  while(m>0 && src[m-1] != '/' && src[m-1] != '\\' && src[m-1] != ':')--m;
  if(n==0 && m==0) return sm_string(src);
  else return sm_string(src+m,n-m);
}

sm_string dirname(char const *src)
{
  int n = std::strlen(src);
  while(n>0 && (src[n-1] == '/' || src[n-1] == '\\' || src[n-1] == ':'))--n;
  int m = n;
  while(m>0 && src[m-1] != '/' && src[m-1] != '\\' && src[m-1] != ':')--m;
  if(n==0 && m==0) return sm_string("."); // JMS: this is just wrong ..
  else return sm_string(src,m);
}


// I will expand this definition as I go to get more knowledge about
// English irregularities as I need it
sm_string plural(int n, char const *prefix)
{
  if (n==1) {
    return sm_string(prefix);
  }

  if (0==strcmp(prefix, "was")) {
    return sm_string("were");
  }
  if (prefix[std::strlen(prefix)-1] == 'y') {
    return sm_stringc << sm_string(prefix, std::strlen(prefix)-1) << "ies";
  }
  else {
    return sm_stringc << prefix << "s";
  }
}

sm_string pluraln(int n, char const *prefix)
{
  return sm_stringc << n << " " << plural(n, prefix);
}


char *copyToStaticBuffer(char const *s)
{
  enum { SZ=200 };
  static char buf[SZ+1];

  int len = std::strlen(s);
  if (len > SZ) len=SZ;
  std::memcpy(buf, s, len);
  buf[len] = 0;

  return buf;
}


bool prefixEquals(char const *str, char const *prefix)
{
  int slen = std::strlen(str);
  int plen = std::strlen(prefix);
  return slen >= plen &&
         0==std::memcmp(str, prefix, plen);
}

bool suffixEquals(char const *str, char const *suffix)
{
  int slen = std::strlen(str);
  int ulen = std::strlen(suffix);    // sUffix
  return slen >= ulen &&
         0==std::memcmp(str+slen-ulen, suffix, ulen);
}


void writeStringToFile(char const *str, char const *fname)
{
  AutoFILE fp(fname, "w");

  if (fputs(str, fp) < 0) {
    xbase("fputs: EOF");
  }
}


sm_string readStringFromFile(char const *fname)
{
  AutoFILE fp(fname, "r");

  sm_stringBuilder sb;

  char buf[4096];
  for (;;) {
    int len = fread(buf, 1, 4096, fp);
    if (len < 0) {
      xbase("fread failed");
    }
    if (len == 0) {
      break;
    }

    sb.append(buf, len);
  }

  return sb;
}


bool readLine(sm_string &dest, FILE *fp)
{
  char buf[80];

  if (!fgets(buf, 80, fp)) {
    return false;
  }

  if (buf[std::strlen(buf)-1] == '\n') {
    // read a newline, we got the whole line
    dest = buf;
    return true;
  }

  // only got part of the sm_string; need to iteratively construct
  sm_stringBuilder sb;
  while (buf[std::strlen(buf)-1] != '\n') {
    sb << buf;
    if (!fgets(buf, 80, fp)) {
      // found eof after partial; return partial *without* eof
      // indication, since we did in fact read something
      break;
    }
  }

  dest = sb;
  return true;
}


sm_string chomp(char const *src)
{
  if (src && src[std::strlen(src)-1] == '\n') {
    return sm_string(src, std::strlen(src)-1);
  }
  else {
    return sm_string(src);
  }
}


// ----------------------- test code -----------------------------
#ifdef TEST_STRUTIL

#include "sm_test.h"
#include <stdio.h>     // printf

void expRangeVector(char const *in, char const *out)
{
  printf("expRangeVector(%s, %s)\n", in, out);
  sm_string result = expandRanges(in);
  xassert(result.equals(out));
}

void trVector(char const *in, char const *srcSpec, char const *destSpec, char const *out)
{
  printf("trVector(%s, %s, %s, %s)\n", in, srcSpec, destSpec, out);
  sm_string result = translate(in, srcSpec, destSpec);
  xassert(result.equals(out));
}

void decodeVector(char const *in, char const *out, int outLen)
{
  printf("decodeVector: \"%s\"\n", in);
  sm_string dest;
  int destLen;
  decodeEscapes(dest, destLen, in, '\0' /*delim, ignored*/, false /*allowNewlines*/);
  xassert(destLen == outLen);
  xassert(0==std::memcmp(out, dest.pcharc(), destLen));
}

void basenameVector(char const *in, char const *out)
{
  printf("basenameVector(%s, %s)\n", in, out);
  sm_string result = sm_basename(in);
  xassert(result.equals(out));
}

void dirnameVector(char const *in, char const *out)
{
  printf("dirnameVector(%s, %s)\n", in, out);
  sm_string result = dirname(in);
  xassert(result.equals(out));
}

void pluralVector(int n, char const *in, char const *out)
{
  printf("pluralVector(%d, %s, %s)\n", n, in, out);
  sm_string result = plural(n, in);
  xassert(result.equals(out));
}


void entry()
{
  expRangeVector("abcd", "abcd");
  expRangeVector("a", "a");
  expRangeVector("a-k", "abcdefghijk");
  expRangeVector("0-9E-Qz", "0123456789EFGHIJKLMNOPQz");

  trVector("foo", "a-z", "A-Z", "FOO");
  trVector("foo BaR", "a-z", "A-Z", "FOO BAR");
  trVector("foo BaR", "m-z", "M-Z", "fOO BaR");

  decodeVector("\\r\\n", "\r\n", 2);
  decodeVector("abc\\0def", "abc\0def", 7);
  decodeVector("\\033", "\033", 1);
  decodeVector("\\x33", "\x33", 1);
  decodeVector("\\?", "?", 1);

  basenameVector("a/b/c", "c");
  basenameVector("abc", "abc");
  basenameVector("/", "");
  basenameVector("a/b/c/", "c");

  dirnameVector("a/b/c", "a/b");
  dirnameVector("a/b/c/", "a/b");
  dirnameVector("/a", "/");
  dirnameVector("abc", ".");
  dirnameVector("/", "/");

  pluralVector(1, "path", "path");
  pluralVector(2, "path", "paths");
  pluralVector(1, "fly", "fly");
  pluralVector(2, "fly", "flies");
  pluralVector(2, "was", "were");
}


USUAL_MAIN

#endif // TEST_STRUTIL
