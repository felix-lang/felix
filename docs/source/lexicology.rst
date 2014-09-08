Lexicology
==========

All Felix files are considered to be UTF-8 encoded Unicode.

Felix uses a scannerless parser, there are no keywords.
Whitespace is generally not significant in Felix, except
of course in strings and where required to separate lexemes.

Comments
--------

Felix provides four kinds of comments.

C style comments using `/*` and `*/` delimiters can be nested.

C++ style comments using `//` run to the end of the line.

C++ style comments using `//$` are comments intended for
user documentation.

In an `fdoc` file, processing begins in document mode,
which includes special documentation markup: the layout
is a minor extension of HTML with some conveniences
for program documentation. Processing switches to Felix
code mode using the `@felix` directive and returns to document
mode with the first subsequent line starting with `@`.

In particular note `fdoc` files can be translated to HTML
by the Felix webserver, and also parsed by the Felix compiler.


Identifiers
-----------

A plain identifier starts with a letter or underscore,
then consists of a sequence of letters, digits, apostrophy, has no more
than one apostrophy or dash in a row, except at the end no dash is
allowed, and any number of apostrophies.

.. code:: felix
   
   Ab_cd1  a' b-x

Identifies starting with underscore are reserved for the implementation.

A letter may be any Unicode character designated for use in an identifier
by the ISO C++ standard. In practice, all high bit set octets are allowed.

A TeX identifier starts with a slosh and is followed by a sequence
of letters. 

TeX Symbols
-----------

`Tex Symbol Reference <http://felix-lang.org/share/src/web/ref/tex_symbols.fdoc>`_

Most mathematical operator symbols in TeX, LaTeX, and AMSTeX may also be 
used in Felix. These are spelled in Unicode with a leading slosh character
followed by a sequence of upper or lower case letters. They are rendered
in HTML documents using the MathJax package.

Integer Literals
----------------


`Integer Literal Reference <http://felix-lang.org/share/lib/grammar/grammar_int_lexer.flxh>`_

An plain integer literal consists of a sequence of digits,
optionally separated by underscores. Each separating
underscore must be between digits.

A prefixed integer literal is a plain integer literal
or a plain integer literal prefixed by a radix specifier.
The radix specifier is a zero followed by one of
the letters ``bBoOdDxX`` for binary, octal, decimal or hex.

An underscore is permitted after the prefix.

The radix is the one specified by the prefix or decimal
by default.

The digits of an integer consist of those permitted
by the radix: ``01`` for binary, ``01234567``
for octal, ``0123456789`` for decimal, ``0123456789abcdefABCDEF``
for hex.

Note there are no negative integer literals.

A type suffix may be added to the end of a prefixed
integer to designate a literal of a particular integer type,
it has the form of an upper or lower case letter or pair of
letters usually combined with a prefix or suffix ``u`` or ``U``
to designate an unsigned variant of the type. 

Signed integers are expected to be two's complement with one
more negative value that positive value. Bitwise and,
or, exclusive or, and complement operations do not apply
with signed types.

The effect of overflow on signed types is unspecified.

Unsigned types use the standard representation. 
Bitwise operations may be applied to unsigned types.
Basic arithmetic operations on unsigned types are
all well defined as the result of the operation
mathematically modulo the maximum value of the type
plus one.

The maximum value of an unsigned type is one less than
two raised to the power of the number of bits in the type.
The number of bits is 8, 16, 32, or 64 or 128 for all unsigned types.

Note that integers starting with 0 are decimal not octal as in C.

A table
of suffices and the types they signify follows in lower case.

====== ========== =================== ===================================================
Suffix  Type      C type              Description
====== ========== =================== ===================================================
i8      int8      int8_t              8 bit signed integer
i16     int16     int16_t             16 bit signed integer
i32     int32     int32_t             32 bit signed integer
i64     int64     int64_t             64 bit signed integer

u8      uint8     uint8_t             8 bit unsigned integer
u16     uint16    uint16_t            16 bit unsigned integer
u32     uint32    uint32_t            32 bit unsigned integer
u64     uint64    uint64_t            64 bit unsigned integer

t       tiny      signed char         C++ signed char used an integer
s       short     short               C short
i       int       int                 C int
l       long      long                C long
ll v    vlong     long long           very long: C long long


ut tu   utiny     unsigned char       unsigned tiny: C++ unsigned char used as an integer
us su   ushort    unsigned short      C unsigned short
u       uint      unsigned            C unsigned int
ul lu   ulong     unsigned long       C unsigned long
ull uv  uvlong    unsigned long long  C unsigned longlong

uz zu   size      size_t              array size
j       intmax    intmax_t            largest integer type
uj ju   uintmax   uintmax_t           largest unsigned integer type
p       intptr    intptr_t            pointer considered as an integer
up pu   uintptr   uintptr_t           pointer considered as an unsigned integer
d       ptrdiff   ptrdiff_t           signed distance between pointers 
ud      uptrdiff  uptrdiff_t          unsigned distance between pointers
====== ========== =================== ===================================================

Note that all these types are distinct unlike C and C++.
The types designated are not the complete set of available
integer like types since not all have literal representations.

Note the suffices do not entirely agree with C.

Floating point Literals
-----------------------

`Reference <http://felix-lang.org/share/lib/grammar/grammar_float_lexer.flxh>`_

Floating point literals follow ISO C89, except that underscores
are allowed between digits, and a a digit is required both before
and after the decimal point if it is present.

The mantissa may be decimal, or hex, a hex mantissa uses a
leading 0x or 0X prefix optionally followed by an underscore.

The exponent may designate a power of 10 using E or e,
or a power of 2, using P or p.

A suffix may be F,f,D,d, L or l, designating floating type,
double precision floating type, or long double precision floating 
type.

.. code:: felix
   
   123.4
   123_456.78
   12.6E-5L
   0xAf.bE6f
   12.7p35


String literals
---------------


`Reference <http://felix-lang.org/share/lib/grammar/grammar_string_lexer.flxh>`_

Generaly we follow Python here.
Felix allows strings to be delimited by: 
single quotes ',
double quotes ",
triped single quotes ''' or
tripled double quotes """.

The single quote forms must be on a single line.

The triple quoted forms may span lines, and include embedded newline
characters.

These forms all allows embedded escape codes.

Raw strings
^^^^^^^^^^^

A prefix "r" or "R" on a double quoted string
or triple double quoted string suppresses escape processing,

this is called a raw string literal.
NOTE: single quoted string cannot be used!

Null terminated strings
^^^^^^^^^^^^^^^^^^^^^^^

A prefix of "c" or "C" specifies a C NTBS (Nul terminated
byte string) be generated instead of a C++ string.
Such a string has type +char rather than string.

Perl interpolation strings
^^^^^^^^^^^^^^^^^^^^^^^^^^

A literal prefixed by "q" or "Q" is a Perl interpolation
string. Such strings are actually functions.
Each occurrence of $(varname) in the string is replaced
at run time by the value "str varname". The type of the
variable must provide an overload of "str" which returns
a C++ string for this to work.

C format strings
^^^^^^^^^^^^^^^^

A literal prefixed by a "f" or "F" is a C format string.

Such strings are actually functions.

The string contains code such as "%d" or other supported
C format specifiers. 

Variable field width specifiers "*" are not permitted. 

The additional format specification %S
is supported and requires a Felix string argument.

Such functions accept a tuple of values like this:

.. code:: felix
   
   f"%d-%S" (42, "Hello")

If ``vsnprintf`` is available on the local platform it is used
to provide an implementation which cannot overrun.
If it is not, ``vsprintf`` is used instead with a 1000 character
buffer.

The argument types and code types are fully checked for type safety.

Special identifiers
^^^^^^^^^^^^^^^^^^^

The special literal with a "n" or "N" prefix is a way to encode
an arbitrary sequence of characters as an identifer in a context
where the parser might interpret it otherwise.
It can be used, for example, to define special characters as functions.
For example:

.. code:: felix
   
   typedef fun n"@" (T:TYPE) : TYPE => cptr[T];

Include Directive
-----------------

An include directive has the syntax:

.. code:: felix
   
   include "filename";

where the filename is a Unix relative filename,
may not have an extension, and may not begin with or 
contain ``..`` (two dots).

If the filename begins with ``./`` then the balance of the name
is relative, a sibling of the including file, otherwise the name
is searched for on an include path. 

In either case, a search succeeds when it finds a file with
the appropriate base path in the search directory with
extension ``.flx`` or ``.fdoc``. If both files exist the
most recently changed one is used. If the time stamps are
the same the choice is unspecified.

