Program structure
=================

A Felix program consists of a nominated root parse unit and
the transitive closure of units with respect to inclusion.

The behaviour of this system consists of the action of the
initialisation code in each unit, performed in sequence
within a given unit, with the order of action between
units unspecified.

Parse Unit
----------

A parse unit is a file augmented by prefixing specified import
files to the front. These consist of a suite of grammar files
defining the syntax and other files defining macros.

By convention import files have the extension ``.flxh``.

With this augmentation all parse units in a program
are independently parsed to produce an list of statements
represented as abstract syntax, denoted an AST (even
though it is a list of trees, not a single tree).

AST
---

The program consists of the concatenation of the ASTs
of each parse unit, resulting in a single AST, which
is then translated to a C++ translation unit by the
compiler.

Grammar
-------

The Felix grammar is part of the library.
It is notionally prefixed to each file to be processed
prior to any import files to specify the syntax
with which the file is to be parsed and translated to
an AST.

The grammar uses an augmented BNF like syntax
with parse actions specified in R5RS Scheme.

The resulting S-expressions are translated to
an intermediate form and then into an internal
AST structure.

The parser is a scannerless GLR parser with significant
extensions.

Grammar syntax
--------------

Not written yet. Browse the 
`grammar directory <http://felix-lang.org/share/lib/grammar>`_
for examples.

Modules
-------

Every Felix program is encapsulated in a module with
the name being a mangle of the basename of the root unit.
The mangling replaces characters in the filename with
other characters so that the module name is a valid
ISO C identifier.

Special procedure ``flx_main``
------------------------------

A program module may contain at most one top level
procedure named ``flx_main``. After initialisation 
code suspends or terminates, this procedure is invoked
if it exists. It is the analogue of ``main`` in C++
however it is rarely used: side-effects of the
root units initialisation code are typically used instead.

Libraries
---------

In Felix a library is a root unit together with its
transitive closure with respect to inclusion,
which does not contain a top level ``flx_main``.

A program unit can be augmented by a set of libraries
which are then considered as if included, but without
an include directive being present.


