Function Application
====================

In an expression, a function `f` can be applied to an argument `x`
like this:

.. code-block:: felix

    f x

This is known as forward, or prefix, application, although it can
also be considered, somewhat whimsically, that the gap between
the `f` and the `x` is an infix operator known as `operator whitespace`.

Although this is the mathematical syntax for application,
many programmers, may prefer to swap the order of the function
and argument like this:

.. code-block:: felix

    x.f

This is known as reverse application. Operator dot binds tighter
than operator whitespace so this expression:

.. code-block:: felix

    g x.f

is parsed as

.. code-block:: felix

    g (x.f)

Both operator dot and operator whitespace are left associative.

Another application operator is stolen from Haskell:

.. code-block:: felix

    h $ g $ h $ x;

Operator `$` binds more weakly then either dot or whitespace,
and is right associative so the above parses as:


.. code-block:: felix

    h (g (h x));

Finally, there is a shortcut for applying a function to the
unit tuple ():

.. code-block:: felix

    #f

which means the same as:

.. code-block:: felix

    f ()

Operator hash `#` is a prefix operator that binds more tightly than
the other application operators.


