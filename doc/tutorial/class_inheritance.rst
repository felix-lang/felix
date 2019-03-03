Inheritance
===========

When defining classes, you can inherit methods from other classes
or specialisations thereof. For example here is a `total order`:

.. code-block::

  class Tord[t]{
    inherit Eq[t];

    virtual fun < : t * t -> bool;
    fun lt (x:t,y:t): bool=> x < y;

    axiom trans(x:t, y:t, z:t): x < y and y < z implies x < z;
    axiom antisym(x:t, y:t): x < y or y < x or x == y;
    axiom reflex(x:t, y:t): x < y and y <= x implies x == y;
    axiom totality(x:t, y:t): x <= y or y <= x;

    fun >(x:t,y:t):bool => y < x;
    fun gt(x:t,y:t):bool => y < x;

    fun <= (x:t,y:t):bool => not (y < x);
    fun le (x:t,y:t):bool => not (y < x);

    fun >= (x:t,y:t):bool => not (x < y);
    fun ge (x:t,y:t):bool => not (x < y);

    fun max(x:t,y:t):t=> if x < y then y else x endif;
    fun \vee(x:t,y:t) => max (x,y);

    fun min(x:t,y:t):t => if x < y then x else y endif;
    fun \wedge(x:t,y:t):t => min (x,y);
  }

The `inherit` statement pulls in the methods of Eq so you can
write:

.. code-block::

  println$ Tord[int]::eq(1,2);

and expect it to work. However when instantiating a total order
you *cannot* provide a definition for inherited methods, you must
provide the instance for the original class:

.. code-block::

  instance Eq[int] { 
    fun ==: int * int -> bool = "$1==$2"; 
  }
  instance Tord[int] { 
    fun <int * int -> bool = "$1<$2"; 
  }

Although in this case, we inherited Eq[t], for all t, we could have
inherited Eq[int], for example. Instances can only be provided for
a class, not a specialisation, because the instances are themselves
defining specialisations.



