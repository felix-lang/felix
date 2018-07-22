Identifiers
===========

Syntax
------

.. code-block:: felix

  syntax felix_ident_lexer {
    /* identifiers */
    regdef ucn =
        "\u" hexdigit hexdigit hexdigit hexdigit
      | "\U" hexdigit hexdigit hexdigit hexdigit hexdigit hexdigit hexdigit hexdigit;

    regdef prime = "'";
    regdef dash = '-';
    regdef idletter = letter | underscore | hichar | ucn;
    regdef alphnum = idletter | digit;
    regdef innerglyph = idletter | digit | dash;
    regdef flx_ident = idletter (innerglyph ? (alphnum | prime) +)* prime* ;
    regdef tex_ident = slosh letter+;
    regdef sym_ident =
      "+" | "-" | "*" | "/" | "%" | "^" | "~" |
      "\&" | "\|" | "\^" |
      /* mutator */
      "&=" | "|=" | "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "<<=" | ">>=" |
      /* comparison */
      "<" | ">" | "==" | "!=" | "<=" | ">=" | "<<" | ">>" | "<>"
    ;

    /* NOTE: upgrade to support n"wird + name" strings */
    literal flx_ident =># "(utf8->ucn _1)";
    literal tex_ident =># "_1";
    literal sym_ident =># "_1";

    sname := flx_ident =># "_1" | tex_ident =># "_1" | sym_ident =># "_1";

  }

Description
-----------

C like identifiers
++++++++++++++++++

Felix has several forms of identifier. The C like identifier starts with
a letter or underscore, and is followed by a sequence of letters, underscores
digits, a hyphen, or a single quote mark. The letters can be any unicode code point
allowed in ISO C++. Only ASCII digits are allowed.

Names starting with one or more underscores are reserved for the system.
A unicode escape `\uFFFF` or `\uFFFFFFFF` may also be used and is translated
to the UTF-8 byte sequence.

Tex Identifiers
+++++++++++++++

Felix also allows any TeX identifier, a slosh `\` followed by a sequence
of ASCII letters.

N strings
+++++++++

The string form consisting of an `n` or `N` followed by
a short string literal specifies the contents of the short string
literal should be considered as an identifier. This is useful in two 
places: first, where the identifier is required to have a certain spelling so it can
bind to C or C++ code.
and secondly when an identifier is required but the parser would
recognise it as a keyword in that context. Note that Felix has no keywords,
identifiers are simply recognised as keyword like tokens in certain contexts,
and not others. For example:

.. code-block:: felix

   var var = 1; // the first var is a keyword, the second an identifier
   n"var" = 2;  // var = 2 would be a syntax error


Special Symbols
+++++++++++++++

Certain operators are recognised as identifiers to allow defining
them as ordinary functions; from the specification:

.. code-block:: felix

      "+" | "-" | "*" | "/" | "%" | "^" | "~" |
      "\&" | "\|" | "\^" |
      /* mutator */
      "&=" | "|=" | "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "<<=" | ">>=" |
      /* comparison */
      "<" | ">" | "==" | "!=" | "<=" | ">=" | "<<" | ">>" | "<>"





