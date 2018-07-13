Logic Type Bool
===============

Felix provides a type for simple logic which tradiationally
is called `bool` after mathematician George Bool.

Type
----

In Felix, `bool` is a special case of a more general mechanism
we will meet later. It is an `alias` for the type `2`, which is
the type that handles two alternatives:

.. code-block:: felix

    typedef bool = 2;

The `typedef` binder binds a name to an existing type,
that is, it creates an alias.

Constants
---------

There are two predefined constants of type bool, `true` and `false`.

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


Summary: Logical operations
---------------------------

========     ===================     =======  =============
Operator     Type                    Syntax   Semantics
========     ===================     =======  =============
or           bool * bool -> bool     Infix    Disjunction
and          bool * bool -> bool     Infix    Conjunction
not          bool -> bool            Prefix   Negation
========     ===================     =======  =============



