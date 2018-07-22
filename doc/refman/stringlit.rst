String Forms
============

Syntax
------

.. code-block:: felix

  SCHEME """
  (define (decode-string s)
    (begin
      (adjust-linecount s)
      (let*
        (
          (n (string-length s))
          (result
            (cond
              ((prefix? "w'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "W'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "c'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "C'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "u'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "U'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "f'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "F'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "q'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "Q'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "n'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "N'''" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "r'''" s)(substring s 4 (- n 3)))
              ((prefix? "R'''" s)(substring s 4 (- n 3)))
              ((prefix? "'''" s)(unescape (substring s 3 (- n 3))))

              ((prefix? "w\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "W\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "c\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "C\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "u\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "U\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "f\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "F\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "q\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "Q\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "n\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "N\"\"\"" s)(unescape (substring s 4 (- n 3))))
              ((prefix? "r\"\"\"" s)(substring s 4 (- n 3)))
              ((prefix? "R\"\"\"" s)(substring s 4 (- n 3)))
              ((prefix? "\"\"\"" s)(unescape (substring s 3 (- n 3))))

              ((prefix? "w'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "W'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "c'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "C'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "u'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "U'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "f'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "F'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "q'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "Q'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "n'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "N'" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "r'" s)(substring s 2 (- n 1)))
              ((prefix? "R'" s)(substring s 2 (- n 1)))
              ((prefix? "'" s)(unescape (substring s 1 (- n 1))))

              ((prefix? "w\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "W\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "c\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "C\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "u\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "U\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "f\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "F\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "q\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "Q\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "n\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "N\"" s)(unescape (substring s 2 (- n 1))))
              ((prefix? "r\"" s)(substring s 2 (- n 1)))
              ((prefix? "R\"" s)(substring s 2 (- n 1)))
              ((prefix? "\"" s)(unescape (substring s 1 (- n 1))))

              (else error)
            )
          )
        )
        ;;(begin
        ;;   (newline)(display "string=")(display s)
        ;;   (newline)(display "text=")(display result)
           result
        ;;)
      )
    )
  )
  """;

  // Scheme string to Felix string literal
  SCHEME """
  (define (strlit s)
      `(ast_literal ,_sr "string" ,s ,(string-append "::std::string(" (c-quote-string s) ")"))
  )
  """;

  //$ String literals.
  //$
  //$ Generaly we follow Python here.
  //$ Felix allows strings to be delimited by;
  //$
  //$ single quotes '
  //$ double quotes "
  //$ triped single quotes '''
  //$ tripled double quotes """
  //$
  //$ The single quote forms must be on a single line.
  //$ The triple quoted forms may span lines, and include embedded newline
  //$ characters.
  //$
  //$ These forms all allows embedded escape codes.
  //$ These are:
  //$
  //$  \a  -  7 : bell
  //$  \b  -  8 : backspace
  //$  \t  -  9 : horizontal tab
  //$  \n  - 10 : linefeed, newline
  //$  \r  - 13 : carriage return
  //$  \v  - 11 : vertical tab
  //$  \f  - 12 :form feed
  //$  \e  - 27 : escape
  //$  \\  - \  : slosh
  //$  \"  - "  : double quote
  //$  \'  - '  : single quote
  //$  \   - 32 : space
  //$
  //$  \xFF - hexadecimal character code
  //$  \o7 \o77 \o777 -- octal character code (stops on count of 3 or non-octal character)
  //$  \d9 \d99 \d999 -- decimal character code (stops on count of 3 or non-decimal character)
  //$  \uFFFF - utf8 encoding of specified hex value
  //$  \UFFFFFFFF - utf8 encoding of specified hex value
  //$
  //$ A prefix "r" or "R" on a double quoted string
  //$ or triple double quoted string suppresses escape processing,
  //$ this is called a raw string literal.
  //$ NOTE: single quoted string cannot be used!
  //$
  //$ A prefix "w" or "W" specifies a wide character string,
  //$ of character type wchar. DEPRECATED.
  //$
  //$ A prefix of "u" or "U" specifes a string of uint32.
  //$ This is a full Unicode string.
  //$ THIS FEATURE WILL BE DEPRECATED.
  //$ IT WILL BE REPLACED BY C++11 Unicode compliant strings.
  //$
  //$ A prefix of "c" or "C" specifies a C NTBS (Nul terminated
  //$ byte string) be generated instead of a C++ string.
  //$ Such a string has type +char rather than string.
  //$
  //$ A literal prefixed by "q" or "Q" is a Perl interpolation
  //$ string. Such strings are actually functions.
  //$ Each occurrence of $(varname) in the string is replaced
  //$ at run time by the value "str varname". The type of the
  //$ variable must provide an overload of "str" which returns
  //$ a C++ string for this to work.
  //$
  //$ A literal prefixed by a "f" or "F" is a C format string.
  //$ Such strings are actually functions.
  //$ The string contains code such as "%d" or other supported
  //$ C format string. Variable field width specifiers "*" are
  //$ not permitted. The additional format specification %S
  //$ is supported and requires a C++ string argument.
  //$ Such functions accept a tuple of values like this:
  //$
  //$ f"%d-%S" (42, "Hello")
  //$
  //$ If vsnprintf is available on the local platform it is used
  //$ to provide an implementation which cannot overrun.
  //$ If it is not, vsprintf is used instead with a 1000 character
  //$ buffer.
  //$
  //$ The argument types and code types are fully checked for type safety.
  //$
  //$ The special literal with a "n" or "N" prefix is a way to encode
  //$ an arbitrary sequence of characters as an identifer in a context
  //$ where the parser might interpret it otherwise.
  //$ It can be used, for example, to define special characters as functions.
  //$ For example:
  //$
  //$ typedef fun n"@" (T:TYPE) : TYPE => cptr[T];
  //$
  syntax felix_string_lexer {
    /* Python strings */
    regdef qqq = quote quote quote;
    regdef ddd = dquote dquote dquote;

    regdef escape = slosh _;

    regdef dddnormal = ordinary | hash | quote | escape | white | newline;
    regdef dddspecial = dddnormal | dquote dddnormal | dquote dquote dddnormal;

    regdef qqqnormal = ordinary | hash | dquote | escape | white | newline;
    regdef qqqspecial = qqqnormal | quote qqqnormal | quote quote qqqnormal;

    regdef qstring_tail = (ordinary | hash | dquote | escape | white) * quote;
    regdef dstring_tail = (ordinary | hash | quote | escape | white) * dquote;
    regdef qqqstring_tail = qqqspecial * qqq;
    regdef dddstring_tail = dddspecial * ddd;

    regdef qstring = quote qstring_tail;
    regdef dstring = dquote dstring_tail;
    regdef qqqstring = qqq qqqstring_tail;
    regdef dddstring = ddd dddstring_tail;


    regdef raw_dddnormal = ordinary | hash | quote | slosh | white | newline;
    regdef raw_dddspecial = raw_dddnormal | dquote raw_dddnormal | dquote dquote raw_dddnormal;

    regdef raw_qqqnormal = ordinary | hash | dquote | slosh | space | newline;
    regdef raw_qqqspecial = raw_qqqnormal | quote raw_qqqnormal | quote quote raw_qqqnormal;

    regdef raw = 'r' | 'R';

    regdef raw_dstring_tail =  (ordinary | hash | quote | escape | white) * dquote;
    regdef raw_qqqstring_tail = raw_qqqspecial * qqq;
    regdef raw_dddstring_tail = raw_dddspecial * ddd;

    regdef raw_dstring = raw dquote dstring_tail;
    regdef raw_qqqstring = raw qqq qqqstring_tail;
    regdef raw_dddstring = raw ddd dddstring_tail;

    regdef plain_string_literal = dstring | qqqstring | dddstring;
    regdef raw_string_literal = raw_dstring | raw_qqqstring | raw_dddstring;

    regdef string_literal = plain_string_literal | qstring | raw_string_literal;

    regdef wstring_literal = ('w' | 'W') plain_string_literal;
    regdef ustring_literal = ('u' | 'U') plain_string_literal;
    regdef cstring_literal = ('c' | 'C') plain_string_literal;
    regdef qstring_literal = ('q' | 'Q') plain_string_literal;
    regdef fstring_literal = ('f' | 'F') plain_string_literal;
    regdef nstring_literal = ('n' | 'N') plain_string_literal;

     // String as name.
    literal nstring_literal =># "(decode-string _1)";
    sname := nstring_literal =># "_1";

    // String for pattern or code template.
    regdef sstring = string_literal;
    literal sstring =># "(decode-string _1)";

    // Cstring for code.
    regdef scstring = cstring_literal;
    literal scstring =># "(decode-string _1)";

    // String for string parser.
    regdef strstring = string_literal;
    literal strstring =># "(c-quote-string (decode-string _1))";

    // String like literals.
    regdef String = string_literal;
    literal String =># """
      (let*
        (
          (ftype "string")
          (iv (decode-string _1))
          (cv (c-quote-string iv))
          (cv (string-append "::std::string(" cv ")"))
        )
        `(ast_literal ,_sr ,ftype ,iv ,cv)
      )
    """;
    sliteral := String =># "_1";

    regdef Wstring = wstring_literal;
    literal Wstring =># """
      (let*
        (
          (ftype "wstring")
          (iv (decode-string _1))
          (cv (c-quote-string iv))
          (cv (string-append "wstring(" cv ")"))
        )
        `(ast_literal ,_sr ,ftype ,iv ,cv)
      )
    """;
    sliteral := Wstring =># "_1";

    regdef Ustring = ustring_literal;
    literal Ustring =># """
      (let*
        (
          (ftype "ustring")
          (iv (decode-string _1))
          (cv (c-quote-string iv))
          (cv (string-append "ustring(" cv ")"))
        )
        `(ast_literal ,_sr ,ftype ,iv ,cv)
      )
    """;
    sliteral := Ustring =># "_1";

    regdef Cstring = cstring_literal;
    literal Cstring =>#
    """
      (let*
        (
          (ftype "cstring")
          (iv (decode-string _1))
          (cv (c-quote-string iv))
        )
        `(ast_literal ,_sr ,ftype ,iv ,cv)
      )
    """;
    sliteral := Cstring =># "_1";

    regdef Qstring = qstring_literal;
    literal Qstring =># "`(ast_interpolate ,_sr ,(decode-string _1))";
    sliteral := Qstring =># "_1";

    regdef Fstring = fstring_literal;
    literal Fstring =># "`(ast_vsprintf ,_sr ,(decode-string _1))";
    sliteral := Fstring =># "_1";

  }

Description
-----------

Felix provides string like literals with several roles:

  * strings
  * C strings
  * arbitrary identifiers
  * formatting functions
  * interpolation strings

Short Literal
+++++++++++++

A basic string literal consists of a quote `'`, some text excluding a quote, and a terminating
quote, all on one line, or, a double quote `"`, some text excluding a double quote,
and a terminating double quote, all on one line. The text consists of UTF-8 encoded
Unicode and should not contain any code points below space (0-0x1F). The system
does not check the validity of the UTF-8 encoding or code points represented.

String literals have type `string` in Felix and represented by C++ `::basic_string<char>`.


Long Literal
++++++++++++

A long literal consist of three quotes `'''`, some text which may include the end of a line,
does not contain three quotes, and is terminated by three quotes, or, three double
quotes `"""`, some text excluding three double quotes, which may span multiple
lines, terminated by three double quotes. The system does not check the validity of
the UTF-8 encoding or code points represented. Long literals are sometimes
called triple quoted strings.

Escape Codes
++++++++++++

Short and long literals may include escape codes. Although most of the literal
text is processed as written, escape codes are translated to other sequences
of bytes. An escape code consists of a slosh '\\` character and some following
characters.

C Escapes
^^^^^^^^^

=========== ==========   ===============
Code        Translation  Meaning
=========== ==========   ===============
\\a         0x07 LF      bell
\\a         0x08 BS      backspace
\\t         0x09 HT      horizontal tab
\\n         0x0A LF      line feed, end line 
\\v         0x0B VT      vertical tab
\\f         0x0C FF      form feed
\\r         0x0D CR      carriage return 
\\e         0x1B ESC     escape
\\\\        \\           slosh
\"          "            double quote
\'          '            single quote
\                        space
\d9                      decimal escape
\d99                     decimal escape
\d999                    decimal escape
\o7                      octal escape
\o77                     octal escape
\o777                    octal escape
\0xF                     hex escape
\0xFF                    hex escape
\uFFFF                   short unicode escape
\UFFFFFFFF               long unicode escape
=========== ==========   ===============

Numeric Escapes
^^^^^^^^^^^^^^^

Numeric escapes start with `\d`, `\o`, or `\x` followed by 
digits in decimal, octal, or hex radices respectively.
Hex letters can be upper or lower case. The escape
is terminated by either a non-radix character, or
the maximum number allowed digits: 3,3 and 2 respectively.

Unicode Escapes
^^^^^^^^^^^^^^^

A unicode escape is `\u` and exactly 4 hex digits
or `\U` and exactly 8 hex digits. The hex encoding
is translated to an integer and then the escape
is replaced by the UTF-8 representation of that
code point. Felix uses a full UTF-8 encoding so 
up to 2^32 values of 1 to 5 bytes may be generated.

Raw Strings
+++++++++++

A short or long string literal using double quote delimiters
may be prefixed by `r` or `R` indicating a raw string, in which
escape codes are not recognised. Note the prefix cannot
be used for single quoted strings because single quotes
are allowed in identifiers.

Raw strings are mainly used for regular expression strings
because regular expression encodings contain a lot of 
special characters, particularly sloshes: without raw
strings each slosh would have to be encoded as two sloshes.
They're also useful on Windows where slosh is a path separator.

C strings
+++++++++

A string of type `cstring` which is represented by an array of
characters in C (that is, a pointer to a char) can be created
by prefixing a string literal with `c`.



Identifiers
+++++++++++

An identifier can be written as a string prefixed by `n`.
This is useful if an identifier would not be recognised
in a certain context. For example:

.. code-block:: felix

  var var = 1;
  n"var" = 2;
 
Format Functions
++++++++++++++++

A string literal prefixed by `f` or '`F` is a format function.
It uses C `printf` like codes and is implemented using
`::std::vsnprintf`. It is first called with a NULL string
and 0 length for the buffer to calculate the required buffer
size, then the buffer is allocated and the function actually run.


Felix also supports a `%S` specifier for C++ strings.
It is converted to `%s` and the internal char array of the 
C++ string used as an argument.

The `*` format specifier is not supported.

The compiler scans the string to calculate the type
of the arguments. The arguments must be presented as
a tuple.

.. code-block:: felix

   println$ f'Hello %S on %d' ("Felix", 42);


====== ========
Code   Type
====== ========
hhd    tiny
hhi    tiny
hho    utiny
hhx    utiny
hhX    utiny

hd     short
hi     short
hu     ushort
ho     ushort
hx     ushort
hX     ushort

d      int
i      int
u      uint
o      uint
x      uint
X      uint

ld     long
li     long
lu     ulong
lo     ulong
lx     ulong
lX     ulong

lld    vlong
lli    vlong
llu    uvlong
llo    uvlong
llx    uvlong
llX    uvlong

zd     ssize
zi     ssize
zu     size
zo     size
zx     size
zX     size

jd     intmax
ji     intmax
ju     uintmax
jo     uintmax
jx     uintmax
jX     uintmax


td     ptrdiff
ti     ptrdiff
tu     uptrdiff
to     uptrdiff
tx     uptrdiff
tX     uptrdiff

e      double
E      double
f      double
F      double
g      double
G      double
a      double
A      double

Le     ldouble
LE     ldouble
Lf     ldouble
LF     ldouble
Lg     ldouble
LG     ldouble
La     ldouble
LA     ldouble

c      int

S      string
s      &char
p      address
P      address
====== ========


Interpolation Strings
+++++++++++++++++++++

A string prefixed by a `q` or `Q` is an interpolation string.
Such a string may contain `$(varname)` where varname is a visible
variable name. The code is replaced by `%S`, and a tuple whose components
are the application of the `str` function to the variable is formed and
the string, considered now as a format function literal, is applied to it.

.. code-block:: felix

  var x = 1; var y = 2;
  println$ q"$(x) + $(y)";



Constant Folding
++++++++++++++++

Felix compiler concatenates adjacent string literals.
So for example you can do this:

.. code-block:: felix

   var x = 
     "To be\n"
     "Or not to be\n"
     "That is the question\n"
   ;



