Tuples
======

A tuple is a heterogenous array.
It is a sequence of values of any type.

Type
++++

The type of a tuple is written as a product of the
types of the components:

.. code-block:: felix

    int * string * double

Values
++++++

A tuple value is written as a comma separated sequence:

.. code-block:: felix

    var x : int * string * double = 1, "hello", 4.2;

If all the components have the same type, you get
an array instead.

Projections
+++++++++++

A tuple projection is like an array projection except
that only  literal integer index is allowed. This is
so that the type is known. The indices are zero origin,
as for arrays.

.. code-block:: felix

    var x : int * string * double = 1, "hello", 4.2;
    println$ x.1; // string


Unit Tuple
----------

There is a special tuple with no components. It is given
the type `1` or `unit`. The value is written `()`.




