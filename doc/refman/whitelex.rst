Overall Lexical Structure
=========================

Felix supports 3 kinds of source files.

flx files
---------

These files have `*.flx` extension. They are the primary kind
of code only file.

Felix is a free form language meaning end of lines are generally
not significant. White space can be freely inserted between significant
lexemes, and sometimes it is required to separate them correctly.

There are two forms of lexical comments. Felix allows the usual
C++ style `// to eol` comments and C style `/* to */` comments,
except that C style comments can be nested.

Continuation lines are not supported.

Felix does not have any keywords. Instead in certain contexts,
certain identifiers are treated as keywords. The same words
are ordinary identifiers in other contexts.

The Felix grammar is specified in the library, so it can
be easily extended by the programmer. When adding new grammar,
avoid misusing symbols such as `;` or `,`, as this may either
fail or make the code unreadable.

Identifiers, integer literals, floating literals, and string
literals, are also defined in the library in the grammar.

fdoc files
----------

A second form of file uses the `*.fdoc` extension. These files
are used for test cases. 

Fdoc files start with commentary, in *prose* mode. You can write
any text, as if in a comment. Special lines are allowed to 
support typesetting and interpretation, switching to *code*
mode, and specifying the output in *expect* mode:

.. code-block:: felix

  @title Name of the Test
  @h1 A heading
  @h2 A subheading
  Now we will check some things.
  @felix
  fun f() => 1;
  println$ f();
  @
  @doc
  This should print just 1.
  @expect
  1
  @


The `@felix` line switches to code mode, and any line starting with `@` ends code
mode. The `@expect` line allows the expected output to be provided.

This form of code can be run exactly like ordinary `*.flx` files.

However, the `flx` tool can also run and check a whole test suite from a single
command line. 


fdoc packages
-------------

There is an advanced form of `*.fdoc` files which provide a set of code
files. The files have to be extracted with a special tool, `flx_iscr`.
Most of the Felix system is stored on GitHub as packages. Packages
allow collections or related files to be put together along with
documentation.
