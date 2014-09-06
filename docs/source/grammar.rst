Grammar
=======

The grammar consists of a fixed core grammar
described briefly here which is used to define the rest
of the language in the library.

The core parser is a scannerless Generalised LR parser
with significant extensions, Dypgen, which actions hard
coded in Ocaml, which is used to interpret syntax in an
extended BNF format with action codes in using OCS
Scheme an R5RS implementation with a few modifications.

The interpreter builds an automaton for the extended
grammar which is then used to translate Felix code
into an AST form via Scheme S-expressions returned by
the actions.

DSSLs
-----

The grammar is divided into sets of semantically related
productions which form Domain Specific Sub Languages
or DSSLs. 

Syntax statement
^^^^^^^^^^^^^^^^

A DSSL is defined by the syntax statement, which
contains grammar productions.

priority statement
^^^^^^^^^^^^^^^^^^

The priority statement names a list of totally ordered
symbols used for precedence control of indexed non-terminals.

requires statement
^^^^^^^^^^^^^^^^^^

The requires statement documents the DSSLs required
by its containing DSSL.

production statement
^^^^^^^^^^^^^^^^^^^^

A production is of a form similar to this:

.. code-block:: felix

  sthe_name := "label_address" sname =># "`(ast_label_ref ,_sr ,_2)";

The nonterminal being defined is first, then `:=` followed by a sequence
of literals, non-terminals, or meta symbols, then `=>#` followed by executable Scheme
code expressed as a string which is evaluated to an S-expression, 
finally a terminating `;`.

standard non-terminals
^^^^^^^^^^^^^^^^^^^^^^

The core non-terminals are `sexpr` for an expression,
`stmt` for a statement.

meta symbols
^^^^^^^^^^^^

Parenthesis are used for grouping, juxtaposition for
sequencing, `+` for 1 or more repetitions, `*` for
0 or more repetations, postfix `?` for 0 or 1 repetations.

open syntax statement
^^^^^^^^^^^^^^^^^^^^^

DSSLs are not available for parsing until named
in an open syntax statement.

