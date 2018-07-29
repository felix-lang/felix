Kinding System
==============

Felix has a basic kinding system. There is one builtin kind, namely `TYPE`,
the kind of all types.

There are two kind constructors. The infix binary function constructor `->` can be used
to denote the kind of a type function. The chained non-associative n-ary product
constructor `*` can be used to denote the kind of a type tuple.

Kinds are required in some circumstances, for example Monads are parametrised
by a type function:

.. code-block:: felix

  class Monad [M: TYPE->TYPE] {
    virtual fun ret[a]: a -> M a;
    virtual fun bind[a,b]: M a * (a -> M b) -> M b;
    fun join[a] (n: M (M a)): M a => bind (n , (fun (x:M a):M a=>x));
  }

A suitable function is:

.. code-block:: felix

  typedef fun opt_f (T: TYPE): TYPE => opt[T];

which has kind:

.. code-block:: felix

   TYPE -> TYPE

A kind annotation can be used with a type variable in a polymorphic
specification, if omitted it defaults to `TYPE`. Types also are implicitly
kinded as `TYPE`.

When a list of type variables is given separated by commas each type
is implicitly kinded as TYPE and the whole list is implicitly
kinded as a type tuple. For example in

.. code-block:: felix

  fun f(T,U] (x:T,y:U) => x,y;

the type variable list `T,U` has kind 

.. code-block:: felix

  TYPE * TYPE

Two more kinds will be introduced in the future:

.. code-block:: felix

  BOOL
  INT

These are the kinds of compile time booleans and integers, respectively.










