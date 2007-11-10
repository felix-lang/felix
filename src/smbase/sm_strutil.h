// strutil.h            see license.txt for copyright and terms of use
// various sm_string utilities built upon the 'str' module
// Scott McPeak, July 2000

#ifndef STRUTIL_H
#define STRUTIL_H

#include "sm_str.h"
#include <stdio.h>    // FILE

// direct sm_string replacement, replacing instances of oldstr with newstr
// (newstr may be "")
sm_string replace(char const *src, char const *oldstr, char const *newstr);

// works like unix "tr": the source sm_string is translated character-by-character,
// with occurrences of 'srcchars' replaced by corresponding characters from
// 'destchars'; further, either set may use the "X-Y" notation to denote a
// range of characters from X to Y
sm_string translate(char const *src, char const *srcchars, char const *destchars);

// a simple example of using translate; it was originally inline, but a bug
// in egcs made me move it out of line
sm_string sm_stringToupper(char const *src);
//  { return translate(src, "a-z", "A-Z"); }


// remove any whitespace at the beginning or end of the sm_string
sm_string trimWhitespace(char const *str);


// encode a block of bytes as a sm_string with C backslash escape
// sequences (but without the opening or closing quotes)
sm_string encodeWithEscapes(char const *src, int len);

// safe when the text has no NUL characters
sm_string encodeWithEscapes(char const *src);

// adds the quotes too
sm_string quoted(char const *src);


// decode an escaped sm_string; throw xFormat if there is a problem
// with the escape syntax; if 'delim' is specified, it will also
// make sure there are no unescaped instances of that
void decodeEscapes(sm_string &dest, int &destLen, char const *src,
                   char delim = 0, bool allowNewlines=false);

// given a sm_string with quotes and escapes, yield just the sm_string;
// works if there are no escaped NULs
sm_string parseQuotedString(char const *text);


// this probably belongs in a dedicated module for time/date stuff..
// returns asctime(localtime(time))
sm_string localTimeString();


// given a directory name like "a/b/c", return "c"
// renamed from 'basename' because of conflict with something in sm_string.h
sm_string sm_basename(char const *src);

// given a directory name like "a/b/c", return "a/b"; if 'src' contains
// no slashes at all, return "."
sm_string dirname(char const *src);


// return 'prefix', pluralized if n!=1; for example
//   plural(1, "egg") yields "egg", but
//   plural(2, "egg") yields "eggs";
// it knows about a few irregular pluralizations (see the source),
// and the expectation is I'll add more irregularities as I need them
sm_string plural(int n, char const *prefix);

// same as 'plural', but with the sm_stringized version of the number:
//   pluraln(1, "egg") yields "1 egg", and
//   pluraln(2, "eggs") yields "2 eggs"
sm_string pluraln(int n, char const *prefix);


// Sometimes it's useful to store a sm_string value in a static buffer;
// most often this is so 'gdb' can see the result.  This function just
// copies its input into a static buffer (of unspecified length, but
// it checks bounds internally), and returns a pointer to that buffer.
char *copyToStaticBuffer(char const *src);


// true if the first part of 'str' matches 'prefix'
bool prefixEquals(char const *str, char const *prefix);

// and similar for last part
bool suffixEquals(char const *str, char const *suffix);


// read/write sm_strings <-> files
void writeStringToFile(char const *str, char const *fname);
sm_string readStringFromFile(char const *fname);


// read the next line from a FILE* (e.g. an AutoFILE); the
// newline is returned if it is present (you can use 'chomp'
// to remove it); returns false (and "") on EOF
bool readLine(sm_string &dest, FILE *fp);


// like perl 'chomp': remove a final newline if there is one
sm_string chomp(char const *src);


#endif // STRUTIL_H
