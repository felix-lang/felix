Lexical Structure
=================

Encoding of Program Files
-------------------------

Felix recognises program files consisting of a sequence
of 32 bit code points, encoded by the full, extended,
UTF-8 encoding scheme. Up to 6 bytes may be required
to encode a single code point.

Unicode only requires 21 bits, therefore, UTF-8 encoded
Unicode text will be processed correctly.
 

7-bit ASCII is fully compatible with UTF-8 encoded
Unicode.

Note: on 32 bit machine, only 30 bit code-points can
be processed. This restriction is due to the representation
of integers in Ocaml, the language used for the Felix compiler.

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
