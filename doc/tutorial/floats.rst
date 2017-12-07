Floating Point Numbers
======================

Floating point literals are local approximations to reals.
Local means close to zero. Floats are dense near zero and
loose precision far from it.

Type
----

We lift the main floating point type, `double` from C.

.. code-block:: felix

    type double = "double";

It is a double precision floating point representation
usually conformat to IEEE specifications.

Literals
--------

Floating literals have two parts, a decimal number
known as the mantissa, and a power of 10 known as the
exponent.

.. code-block:: felix

    12.34
    12.34E-4

The mantissa must contain a decimal point with a digit on either
side. The exponent is optional, and consists of the letter `E`
or `e` followed by a small decimal integer literal, or a + sign
or minus sign, and a small decimal integer literal.

If the exponent is present, the mantissa is multiplied by
10 raised to the power of the signed integer part exponent.

Operations
----------

Floating numbers support negation with prefix `-`, addition
with infix `+`, subtraction with infix `-`, multiplication
with infix `*` and division with infix `/` as well as
many other operations given by functions in the library.

It is also possible to perform comparisons, equality `==`,
inequality `!=`, less than `<`, less than or equal to `<=`,
greater than `>` and greater than or equal to `>=`. However
these comparisons reflect floating point arithmentic
which only approximates real arithmetic. Do not be suprised
if the formula

.. code-block:: felix

   1.0 / 3.0 * 3.0 == 1.0

is false. To remedy this properly requires a deep knowledge
of numerical analysis. Felix helps by providing the function
`abs` which can be used like this:

.. code-block:: felix

  abs ( 1.0 / 3.0 * 3.0 - 1.0) < 1.0e-3


to check the result is with about 3 decimal places of 1.0.


Summary: Double Comparisons
---------------------------

========     ======================     =======  =============
Operator     Type                       Syntax   Semantics
========     ===================        =======  =============
==           double * double -> bool    Infix    Equality
!=           double * double -> bool    Infix    Not Equal
<=           double * double -> bool    Infix    Less or Equal
<            double * double -> bool    Infix    Less
>=           double * double -> bool    Infix    Greater or Equal
>            double * double -> bool    Infix    Greater
========     ======================     =======  =============



Summary: Double Operations
---------------------------

========     ==========================   =======  =============
Operator     Type                         Syntax   Semantics
========     ==========================   =======  =============
\+           double * double -> double    Infix    Addition
\-           double * double -> double    Infix    Subtraction 
\*           double * double -> double    Infix    Multiplication
/            double * double -> double    Infix    Division
\-           double -> double             Prefix   Negation
neg          double -> double             Prefix   Negation
abs          double -> double             Prefix   Absolute Value
========     ==========================   =======  =============





