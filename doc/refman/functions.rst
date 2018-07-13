Functions 
=========

Felix function types in a Felix are written:

.. code-block:: felix

   D -> C

where D is the domain type, and C the codomain type, which
may not be 0. 

Functional Definition
---------------------

A Felix function can be written with several forms.
The simplest form is to use an expression to define
the calculation:

.. code-block:: felix

  fun square (x:int): int => x * x;

which has type

.. code-block:: felix

  int -> int

Imperative Definition
---------------------

An expanded form of a function definition uses imperative code:

.. code-block:: felix

   fun pythag(x:double, y:double) = {
      var x2 = x * x;
      var y2 = y * y;
      var h = sqrt (x2 + y2);
      return h;
   }

Purity
------

In Felix functions may depend on variables in a containing scope,
or, store located via a pointer, therefore functions need not
be pure. An adjective can be used to specify a function is pure:

.. code-block:: felix
 
  pure fun twice (x:int) => x + x;

The compiler checks functions to determine if they're pure.
If it finds they are, it adds the pure attribute itself.
It a function is found not to be pure but a pure adjective
is specified, it is a fatal error. If the compiler is unable
to decide if a function is pure, it is assumed to be pure
if and only if the pure adjective is specified.

Side Effects
------------

Functions in Felix are not allowed to have side effects.
The compiler does not enforce this rule.
However the compiler optimises code assuming there are
no side effects in functions, these optimisations are
extensive and pervasive.

It is acceptable to add imperative debugging instrumentation
to functions, because the behaviour in the face of optimisations
is precisely what the debugging instrumentation is designed
to report.

Elision of Constant Functions
-----------------------------

If a function return value is invariant, the compiler
may delete the function and replace applications of it
with the constant returned value. The compiler may or may
not be able to determine the invariance and return value
in general but Felix guarrantees this property in one
very important case: a function with a unit codomain type.

 
