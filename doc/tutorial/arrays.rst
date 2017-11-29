Arrays
======

In Felix, arrays are first class values.

Type
----

An array is given a type consisting of the base type and length.

.. code-block:: felix


    int^4

is a the type of an array of 4 ints. Note, the `4` there is not
an integer but a unitsum type.

Value
-----

An array is given by a list of comma separated expressions:

.. code-block:: felix

    var a :int^4 = 1,2,3,4;

Operations
----------

Projection
++++++++++

The most fundamental operation is the application of a projection
to extract the n'th component of an array. Components are numbered
from 0 up.

.. code-block:: felix

    var a :int^4 = 1,2,3,4;
    for i in 0..<4 do
      println$ a.i;
    done

The projection here is indicated by the `int` i.
An expression can be used provided it is in bounds.

Length
++++++

The length of an array may be obtained with the `len` function.
The value returned is of type `size` which can be converted
to `int` as shown:

.. code-block:: felix

    var x = 1,2,3,4;
    var lx = x.len.int;
    println$ lx; // 4

Value Iteration
+++++++++++++++

A for loop may take an array argument.
The control variable takes on all the values
in the array starting with the first.

.. code-block:: felix

    var x = 1,2,3,4;
    var sum = 0;
    for v in x do
      sum = sum + v;
    done
    println$ sum;






