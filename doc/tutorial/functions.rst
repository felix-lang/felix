Functions
=========

Functions encapsulate calculations.

Type
----

The type of a function with a parameter type D returning a value type C is written

.. code-block:: felix

    D -> C

Definition by Expression
------------------------

A function can be defined by an expression:

.. code-block:: felix

    fun square (x:int) : int => x * x;

or without the return type:

.. code-block:: felix

    fun square (x:int) => x * x;

in which case it is deduced.

Definition by Statements
------------------------

More complex functions can be defined by statements.

.. code-block:: felix

    fun addUp(xs:int^4) : int = {
      var sum = 0;
      for x in xs do
        sum = sum + x;
      done
      return sum;
    }



The return type can be elided:

.. code-block:: felix

    fun addUp(xs:int^4) = {
      var sum = 0;
      for x in xs do
        sum = sum + x;
      done
      return sum;
    }


No side effects
---------------

The effect of a function must be entirely captured in its
returned value; that is, it may not have any side effects.
This asumption is currently not checked, so you could write 
code like this:

.. code-block:: felix

    var mutMe = 0;

    fun addUp(xs:int^4) : int = {
      mutMe = 1;  // bad! 
      var sum = 0;
      for x in xs do
        sum = sum + x;
      done
      return sum;
    }

However, this kind of usage may be useful from time to time,
for example for debugging.

The lack of side effects in a function are used in optimizations,
and the optimizations may have an effect on program behavior.
For example, the following toy program takes the second projection
(``. 1``) on a tuple involving three function calls. Since functions
are assumed to have no side effects, the other function calls 
(``f`` and ``h``) are erased as their return values are never used.


.. code-block:: felix

    fun f(x:int) = {
      println "hi from f!";
      return 2*x;
    }
    fun g(x:int) = {
      println "hi from g!";
      return 3*x;
    }
    fun h(x:int) = {
      println "hi from h!";
      return 4*x;
    }

    val res =  (f 5, g 5, h 5) . 1;
    println res;


The output of the program is just:

.. code-block:: none

    hi from g!
    15

Purity
------
Functions can further be annotated to be ``pure`` or ``impure``, but at the 
moment, the semantics of these are not defined and are not checked:

.. code-block:: felix

    pure fun addUp(xs:int^4) : int = {
      // ...
    }

    // or

    impure fun addUp(xs:int^4) : int = {
      // ...
    }    
