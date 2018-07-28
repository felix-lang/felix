Type Functions
==============

A type function can be defined: this is a function which
takes one or more types and returns a type:

.. code-block:: felix

  typedef fun pair_f(T:TYPE,U:TYPE):TYPE  => T * U;
  var x : pair_f (int, long) = 42, 7.2;

Note there is a technical difference between a type schema,
which is an index family of types, and a type function,
which is a higher kind. The entity pair_f has kind:

.. code-block:: felix

  TYPE * TYPE -> TYPE

Since the domain is a pair of types, a type tuple
must be given as an argument. A type tuple is of a
different kind to the type *of* a tuple, which is
merely of kind TYPE.

Type function applications must be resolved during phase 1 lookup.

Functors
--------

If the type function is structurally parametric, it is
also called a functor. Not all type functions are structure
preserving, which is the requirement for a functor.

Functors can be used as arguments to type classes.
For example the library function Monad depends on a functor:

.. code-block:: felix

  class Monad [M: TYPE->TYPE] {
    virtual fun ret[a]: a -> M a;
    virtual fun bind[a,b]: M a * (a -> M b) -> M b;
    fun join[a] (n: M (M a)): M a => bind (n , (fun (x:M a):M a=>x));
  }


 
