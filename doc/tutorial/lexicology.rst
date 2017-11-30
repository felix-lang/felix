Lexical Structure
=================

Encoding of Program Files
-------------------------

Felix recognises program files consisting of a sequence
of 31 bit code points, encoded by the full, extended,
UTF-8 encoding scheme. Up to 5 bytes may be required
to encode a single code point. 

This means the full, original, ISO-10646, and more recent
ISO and Unicode standards are processed, as well as additional
codes above 0x10FFF. Code points up to 0x7FFFF are recognised
and processed.

There is one exception: on 32 bit machines the compiler
only handles 30 bit code points correctly. This is because
Ocaml signed integers on a 32 bit machine are only 31 bits,
and the processing logic assumes the integers are positive.

Program files on systems which use UTF-16 or UCS-4
encoding cannot be processed correctly at this time. 
Please ensure you use UTF-8 encoding for internationalised
Felix programs.

Internet standards require UTF-8 encoding.
UTF-8 is the standard encoding for Linux.
Solaris and Windows tend to use UTF-16.

Do not use Latin-1 encoding.  

The UTF-8 encoding restriction only applies to
program files because decoding of data is up
to the application.

7-bit ASCII is fully compatible with UTF-8 encoded
Unicode.

Reference: <https://unicode-table.com/en/>

Whitespace
----------

Felix allows and ignores spaces between symbols, however
spaces are sometimes required to separate symbols.

All the codes 0x01 through 0x20 inclusive are considered
as white space.

We strongly recommend using *only* the space, SP 0x20,
for horizontal spacing. 
Horizontal tab, HT, code 0x09 are discouraged in all contexts. 

Vertical spacing, or line breaks, should use the LF 0x10
character used on Unix, even on Windows, since most
Windows tools now recognise it. However CR,LF 0x0A, 0x10
is permitted.

Line breaks are lexically significant only in C++ style
comments and strings.

Encoding of Output
------------------

Felix generates C++ which is encoded with UTF-8.

Additional encoding such as Unicode
escapes may be introduced in parts of the output.

All ordinary character strings are emitted as 
single line C++ strings using only one byte
encoding of code points 0x20-0x7E. All other
code points are represented either by C++
escapes, such as `\\n` for newline, or hex
escapes `\\x` with exactly two hex digits,
representing the UTF-8 encoded code point
with a sequence of hex escapes.

The target C++ compiler must recognise and correctly
process programs according to ISO C++ lexical standards
and support UTF-8 encoding.
