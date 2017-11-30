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

    fun addup(a:int^4) : int = {
      var sum = 0;
      for i in x do
         sum = sum + 1;
      done
      return sum;
    }

The return type can be elided:

.. code-block:: felix

    fun addup(a:int^4) = {
      var sum = 0;
      for i in x do
         sum = sum + 1;
      done
      return sum;
    }



No side effects
---------------

The effect of a function must be entirely captured in its
returned value; that is, it may not have any side effects.

 
