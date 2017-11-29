Logic Type Bool
===============

Felix provides a type for simple logic which tradiationally
is called `bool` after mathematician George Bool.

Type
----

We lift the type from C++ as usual:

.. code-block:: felix

    type bool = "std::bool";


Constants
---------

There two predefined constants, `true` and `false`.

Operations
----------

The prefix operator `not` provides negation, infix
`and` conjunction, and infix `or` disjunction,
with weak precedences, of decreasing strength.

.. code-block:: felix

    not a and b or c

is parsed as

.. code-block:: felix

    ((not a) and b) or c


These operators are all weaker than the comparisons they often
take as arguments, so that


.. code-block:: felix
 
    a < b and b < c

is parsed as

.. code-block:: felix
 
    (a < b) and (b < c)





