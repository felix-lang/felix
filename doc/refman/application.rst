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


Another application for binary operator is

.. code-block:: felix

  a `(f) b // means f (a,b)

Overloading
-----------

There are two kinds of application: a *direct* application and an *indirect*
application. A direct application is the most common kind and involves
applying a function by name to an argument expression:

.. code-block:: felix

  fun f(x:int) => x;
  fun f(y:double) => y;
  println$ f 1;

In this case, there are two visible functions named `f`, the first
one is selected because its domain has the same type as the argument,
namely `int`.

The algorithm which selects the function to use is called overload
resolution.

Indirect Application
--------------------

When an expression other than a function name is applied,
there are two cases: *normal* indirect application or *special*
application.

If the expression has function type, the expression represents
a function closure rather than a function. The argument it
is applied to must match the domain of the function type:

.. code-block:: felix

  fun f(x:int) => x;
  var g = f;
  println$ g 1;

Special Apply
-------------

If the expression being applied has type T which is not a function type,
then Felix instead looks for a function named `apply` which takes a tuple
of type `T * A` where `A` is the type of the argument. For example:

.. code-block:: felix

  fun apply (x:string, y:string) => x + y;
  println$ "Hello " "World";

Here is a string is applied to a string. Since a string isn't a function,
Felix looks for and finds a function named `apply` with domain `string * string`.





