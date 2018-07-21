Applications
============

Syntax
------
 
.. code-block:: felix

  x[sdollar_apply_pri] := x[stuple_pri] "$" x[sdollar_apply_pri] 
  x[spipe_apply_pri] := x[spipe_apply_pri] "|>" x[stuple_pri] 
  x[stuple_pri]  := x[stuple_pri] "`(" sexpr ")" sexpr
  x[sapplication_pri] := x[sapplication_pri] x[>sapplication_pri] 
  x[sfactor_pri] := x[sfactor_pri] "." x[>sfactor_pri] 
  x[sfactor_pri] := x[sfactor_pri] "*." x[>sfactor_pri]
  x[sfactor_pri] := x[sfactor_pri] "&." x[>sfactor_pri]
  x[sthename_pri] := "#" x[sthename_pri] 

Description
-----------

Felix has a large number of application operators, from
highest precedence to lowest:

.. code-block:: felix

  f$a    // operator dollar, right associative
  a|>f   // operator pipe apply: reverse application, left associative
  f a    // operator whitespace, left associative
  a.f    // operator dot: reverse application, left associative
  a*.f   // means (*a).f
  a&.f   // means (&a).f
  #f     // constant evaluator: means f ()

There are also two special combinations:

.. code-block:: felix

The rich collection of operators is indended to reduce the
need for parentheses.

Another application for binary operator is

.. code-block:: felix

  a `(f) b // means f (a,b)

