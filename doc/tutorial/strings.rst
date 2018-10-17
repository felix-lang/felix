Strings
=======

A string is basically a sequence of characters
with a definite length. The type is the traditional C++ 
string type, and supports Unicode only by UTF-8 encoding.

Strings are 8 bit clean, meaning all 256 characters, 
including nul, may occur in them.

However string literals may not directly contain a nul.
String literals are actually C nul terminated char strings
which are lifted to C++ automatically.

Type
----

The string type in Felix is the based
on C++:

.. code-block:: felix

    type string = "::std::basic_string<char>"
      requires Cxx_headers::string
    ;

Literals
--------

There are two simple forms of string literals:

.. code-block:: felix

    var x = "I am a string";
    var y = 'I am a string too';

using double quote delimiters and single quote delimiters.
These literals cannot exceed a single line.  However
you can concatenate them by juxtaposition:

.. code-block:: felix

    var verse = 
      "I am the first line,\n"
      'and I am the second.'
    ;

Notice the special encoding `\\n` which inserts an end of
line character into the string rather than a `\\` followed
by an `n`. This is called an escape.

You can prevent escapes being translated with raw
strings like this:

.. code-block:: felix

    r"This \n is retained as two chars"

Only double quoted strings can be raw.

Felix also has triple quoted strings, which span
line boundaries, and include end of lines in the
literal:

.. code-block:: felix
    """This is
    a very long 
    string"""

which contains two end of line characters in the string, whilst
this one:

.. code-block:: felix

    '''
    Here is another
    long string
    '''

has three end of lines (one after the first triple quote).

Length
------

Use the `len` function:

.. code-block:: felix

   var x = "hello";
   var y = x.len.int;


Concatenation
-------------

Strings can be concatenated with the infix `+` operator or
just written next to each other, juxtaposition has a higher
precedence than infix `+`.

.. code-block:: felix

    var x = "middle";
    var y = "Start" x + "end";

In this case, the first concatenation of x is done first,
then the second one which appends "end". The result
is independent of the ordering because concatenation
is associative, the run time performance, however, is not,
because concatenation requires coping.

Substring Extraction
--------------------

A substring of a string can be extracted using a slice
with the notation shown:

.. code-block:: felix

    var x = "Hello World";
    var y = x.[3..7]; // 'lo Wo'


Indexing
--------

Select the n'th character:

.. code-block:: felix

    var x = "Hello World";
    var y = x.[1]; // e

Comparisons
-----------

Strings are totally ordered using standard lexicogrphical
ordering and support the usual comparison operators.


Summary: String Comparisons
---------------------------

========     =======================    =======  =================
Operator     Type                       Syntax   Semantics
========     =======================    =======  =================
==           string * string -> bool    Infix    Equality
!=           string * string -> bool    Infix    Not Equal
<=           string * string -> bool    Infix    Less or Equal
<            string * string -> bool    Infix    Less
>=           string * string -> bool    Infix    Greater or Equal
>            string * string -> bool    Infix    Greater
========     =======================    =======  =================



Summary: Double Operations
---------------------------

========     ==========================   =======  =============
Operator     Type                         Syntax   Semantics
========     ==========================   =======  =============
len          string -> size               Prefix   Length
\+           string* string -> string     Infix    Concatenation
\.[_]        string * slice -> string     Postfix  Substring 
\.[_]        string * int -> char         Postfix  Indexing
========     ==========================   =======  =============



