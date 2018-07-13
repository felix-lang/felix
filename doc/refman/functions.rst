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

C bindings
----------

Felix can lift, or bind, a C or C++ function
as a primitive:

.. code-block:: felix

   fun sin: double -> double = "sin($1)"
     requires C_headers::math_h
   ;

  fun arctan2 : double * double -> double =
    "atan2($1,$2)" requires C_headers::math_g
  ;

The special encoding $1, $2 etc refer to components of
the domain tuple. The encoding $a is a quick way to
unpack all the arguments.

[More codes]

C function type
---------------

Felix also has a type for C function values (pointers):

.. code-block:: felix

   D --> C

Do not confuse C function values with Felix functions
specified by C bndings: the latter are first class
Felix functions.

Closures
--------

A felix function can be converted to a closure, which is 
a first class value including both the function and its context.

For example:

.. code-block:: felix

   fun add(x:int) = {
     fun g(y:int) => x + y;
     return g;
   }
   var h = add 1;
   var r = h 2; // r set to 3

In the example, f has type:

.. code-block:: felix

  int -> int -> int

The function arrow is right associative so this means
f accepts an int (x), and returns a function which accepts
another int (y) and returns an int, which is their sum.
The variable h contains a closure of g bound to its context
which contains the variable x, which has been set to 1.

A closure is represent at run time by a pointer to an object
so passing closures around is cheap. Closures are usually
allocated on the heap, which is has a cost. The context
of a closure is a list of the most recent activation records
of the lexically enclosing function frames (ancestors) called a display.
All functions, by default, also include the global data frame,
called the thread-frame (because it is shared by all threads).

CLosures exist at run time and cannot be polymorphic.

Higher Order Functions
----------------------

Functions which accept function arguments, or arguments, or return
function values, are called higher order functions.

A special notation exist for defining a function which returns 
another function:

.. code-block:: felix

  fun add (x:int) (y:int) => y;
  println$ f 1 2;

Here add is a higher order function with arity 2, it has type

.. code-block:: felix

  int -> int -> int

and is equivalent to the previous version of add.

Polymorphic functions
---------------------

Functions support parametric polymorphism:

.. code-block:: felix

  fun swap[T,U] (x:T, y:T) => y,x;

You can also use type class constraints:

.. code-block:: felix

  fun showeol[T with Str[T]] (x:T) => x.str + "\n";

The effect of a type class constraint is to inject the
methods of the class, specialised to the given arguments,
into the scope of the function body. In the example
str is a method of Str which translates a value of type
T into a human readable string.








   
