Macros
======

Felix only provides a rudimentary macro system. Macros are scoped like other
symbols. The primary purpose of macros is to support conditional compilation.
Macros are usually gathered into a single file which is prepended using
a compiler switch to all other files, after parsing.

For advanced synthetic code generation, Felix uses Scheme programming
language in user action codes of the grammar.

Syntax
------

.. code-block:: felix

  stmt := "macro" "val" snames "=" sexpr ";"

  stmt := "forall" sname "in" sexpr "do" stmt* "done"

Semantics
---------

The macro val statement is used to associate a macro name with an expression.
The forall statement is used to generate a sequence of statements
repeatedly replacing occurences of the given name with each of the
given expression in turn, and is useful for table generation.


Constant Folding
----------------

The Felix macro processor performs constant folding for certain types
and operations. 

Expressions involving only boolean constants `true` and `false` and
the logical operations `and`, `or` and `not` are evaluated and simplified.

Adjacent string literals are concatenated.

Conditional Compilation
-----------------------

Felix does not provide any specific directives for conditional
compilation. Instead, conditional expressions and statements
with compile time constant conditions are simplified by the
macro processor, providing conditional compilation using
the same syntax as run time conditionals.

For example:

.. code-block:: felix

  macro val POSIX = true;
  if POSIX do
     typedef file = int;
  else do
     typedef file = HANDLE;
  done

is reduced to:

.. code-block:: felix

  typedef file = int;

by the macro processor, so that even if `HANDLE` is not defined,
there will be no type error because the code using it is eliminated.

The macro processor also reduces pattern matches if possible,
in fact, conditionals are represented by pattern matches!








