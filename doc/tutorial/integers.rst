Integers
========

In Felix we have at type named `int` which has values 
which are small integers close to zero.  

Type
----

This type is
defined by lifting it from C++ in the library by:

.. code-block:: felix

    type int = "int";


The exact range of integers represented is therefore
determined by the underlying C++ implementation.

Literals
--------

Non-negative values of type `int` can be written as a sequence of
the decimal digits like this:

.. code-block:: felix

    0
    1
    23
    42
  
You can also put an underscore between any two digits:

.. code-block:: felix

    123_456

Negation
--------

There are no negative integer literals. However you can
find the negative of an integer using a prefix negation
operator, the dash character `-`, or the function `neg`:

.. code-block:: felix

     -1
     neg 1

Infix Operators
---------------

Integers support simple formulas:

.. code-block:: felix

    (12 + 4 - 7) * 3 / 6 % 2

Here, `+` is addition, infix `-` is subtraction, `*`
is multiplication, `/` is division, and `%` is the 
remainder after a division.

Sign of quotient and remainder
------------------------------

Division and remainder require a little explanation
when negative numbers are involved. The quotient of
a division in C, and thus Felix, always rounds towards
zero, so:

.. code-block:: felix

   -3/2 == 1

The the quotient is non-negative if the two operands
are both negative or both non-negative, otherwise
it is non-positive.

The remainder, then, must satifsy the formula:


.. code-block:: felix

   dividend == quotient * divisor + remainder

so that we have

.. code-block:: felix

   remainder == dividend - quotient * divisor

Therefore the remainder is non-negative if, and only if,
the dividend is non-negative, otherwise it is non-positive.

Comparisons
-----------

We provide the usual comparisons from C: `==` is equality,
`!=` is inequality, `<` is less than, `>` is greater than,
`<=` is less than or equal to, and `>=` is greater than
or equal to.

The result of a comparison is value of `bool` type.

Constant Folding
----------------

If you write a formula involving only literals of type `int`,
the Felix compiler will perform the calculation according
to mathematical rules, using a very much bigger integer
representation. At the end, the result will be converted
back to the smaller `int` representation.

If the result of the calculations exceeds the size of the
compiler internal representation, or, the final result
is to large for an `int`, the result is indeterminate.

Division by Zero
----------------

If a division or remainder operation has a divisor of zero,
the compiler may abort the compilation, or it may defer the
problem until run time. If the problem is deferred and
the code is executed, an exception will be thrown and
the program aborted. However the code may not be executed.

Out of bounds values
--------------------

If the result of a calculation performed at run time
is out of bounds, the result is indeterminate.



