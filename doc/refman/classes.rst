Type Classes
============

Felix has Haskell style type classes which are used to provide
a consistent notational system.

Class definition
----------------

A class is defined like this:

.. code-block:: felix

  class Eq[t] {
    virtual fun == : t * t -> bool;
    virtual fun != (x:t,y:t):bool => not (x == y);

    axiom reflex(x:t): x == x;
    axiom sym(x:t, y:t): (x == y) == (y == x);
    axiom trans(x:t, y:t, z:t): x == y and y == z implies x == z;

    fun eq(x:t, y:t)=> x == y;
    fun ne(x:t, y:t)=> x != y;
    fun \ne(x:t, y:t)=> x != y;
    fun \neq(x:t, y:t)=> x != y;
  }

This class defines the notion of an equivalence relation for some
as yet unspecified type t. It does this by defining two virtual
functions, :code:`==` and :code:`!=` which can be overridden 
in instance specifications for particular types.

The :code:`==` function is pure virtual and must be defined
in an instance for the other functions to work. On the other
hand the :code:`!=` function may be overridden, but if it is
not the specified default will be used.

The non-virtual functions :code:`eq, ne, \ne, \neq` cannot be overridden
and are defined in terms of the virtuals.

This class also specifies axioms which are used to partly define
the required semantics.

Instance definition
-------------------

An instance of our class can be specified like this:

.. code-block:: felix

  instance Eq[int] {
    fun == : int * int -> bool = "$1==$2";
  }

Here, we have defined equality for type :code:`int` and this
also effects the definition of the functions :code:`!=, eq, ne, \ne, \neq`
as well.

Usage
-----

The functions in a class may be refered to by qualified name lookup:

.. code-block:: felix

  println$ Eq::== (1,2);
  println$ Eq::eq (1,2);

Alternatively, you can pass the class dictionary in to a function
or procedure like this:

.. code-block:: felix

  fun eq3[T with Eq[T]] (a:T, b:T, c:T) => a == b and b == c;
  println$ eq3 (42, 42, 42);

The print statement will work because the functions in 
the Eq class are in scope inside the eq3 function,
and there is an instance for type int.

Another way to make the class methods available is to
open the class:

.. code-block:: felix

  open Eq[int];
  println$ 42 == 42 and 42 == 42;

Here the class was opened only for type int.

Resolution
~~~~~~~~~~

Use of class methods is handled in two phases. In the first
phase the methods are chosen by the normal overload
resolution process based on access to the class,
and completely ignoring whether or not appropriate
instances are available.

The second phase occurs during monomorphisation, when
references to virtual functions are replaced by their
overloads from the most specialised instance.
An error diagnostic will be printed and the compilation
terminated if no such instance exists.

Virtual types
=============

A class may also specify a virtual type. This is an existential type
which is dependent on the class type parameters in way which
cannot be specified by a formula. Instead, the type is specified in
each instance. Type parameters are deduced from applications to 
select a class virtual function, a virtual type is used in the
result:

.. code-block:: felix

  class Add[T1, T2] {
     virtual type U;
     virtual fun promote: T1 * T1-> U;
  }

  instance Add[int, long] {
     typedef U = long;
     fun add(x:int,y:long):long = "$1+$2";
  }

Since a virtual type is only known at monomorphisation type
a value of that type can only be used in the argument of a function
which is parametrically polymorphic or a virtual class method.


