Functional Expressions
======================

In Felix, a function is modelled in principle by a C++ class.
A function value, on the other hand, is not a function,
rather it is a called a closure because it captures its environment
and may have internal state. Closures are represented as pointers
to objects of some C++ class.

The distinction is important. In principle in an application `f a`
the `f` is a function value of some suitable type. The type given for
a function in a definition is the type of its closure. However if the
function expression is just a name, overload resolution is performed
to find a suitable function to apply, and the application is direct
so that the function generating the closure being applied is known,
and the application is optimised, for example, by inlining the application.

Function names
--------------

The full name of a function defined by the user can be given
with a suffixed name. For example:

.. code-block:: felix

  class X { fun f(x:int) => x; }
  var g = X::f of int;

The `of` suffix is used in lieu of an argument to perform overload resolution
and select a specific function.

Tuple projections
-----------------

You can name a specific projection of a tuple type by:

.. code-block:: felix

  typedef t = int * long * string;
  var g : t -> string = proj 2 of t;

Array projections
-----------------

.. code-block:: felix

  typedef t = int^5;
  var g : t -> 5 -> int = aproj of t;

[NOTE: THIS ISNT IMPLEMENTED BUT SHOULD BE]


Record and struct projections
-----------------------------

Records and structs use the field name as the name of the projection,
so the usual suffixed form can be used to specify a projection.

.. code-block:: felix

  typedef t = (a:int, b:long, c:string);
  var g : t -> string = a of t;
  struct X { a:int; b:long; c:string);
  var h : X -> string = a of X;

Sum Injections
--------------

cases etc

Union injection
---------------

Polymorphic variant injections
------------------------------


Coarray Injection
-----------------

Compositions
------------

Forward and reverse serial, parallel, mediating morphisms.


//$ Reverse composition
x[srcompose_pri] := x[srcompose_pri] "\odot" x[>srcompose_pri]


Composition Sumary
++++++++++++++++++

There are two composition operators for functions,
both are left associative:

==================== ==================
operator             semantics
==================== ==================
\\circ               forward composition
\\odot               reverse composition
==================== ==================





