Floating Numbers
================

Approximate Reals
-----------------

Felix has three floating types:

==========  =================== =========   =================
Felix       C                   Suffix      Positive Infinity
==========  =================== =========   =================
float       float               f           FINFINITY
double      double float        d           DINFINITY
ldouble     long double float   l           LINFINITY
==========  =================== =========   =================

where the d suffix can be omitted.  The lexical rules are

.. code-block:: felix

  regdef decimal_string = digit (underscore ? digit) *;
  regdef hexadecimal_string = hexdigit (underscore ? hexdigit) *;

  regdef decimal_fractional_constant =
    decimal_string '.' decimal_string;

  regdef hexadecimal_fractional_constant =
    ("0x" |"0X")
    hexadecimal_string '.' hexadecimal_string;

  regdef decimal_exponent = ('E'|'e') ('+'|'-')? decimal_string;
  regdef binary_exponent = ('P'|'p') ('+'|'-')? decimal_string;

  regdef floating_suffix = 'L' | 'l' | 'F' | 'f' | 'D' | 'd';
  regdef floating_literal =
    (
      decimal_fractional_constant decimal_exponent ? |
      hexadecimal_fractional_constant binary_exponent ?
    )
    floating_suffix ?;

This is consistent with ISO C, except that underscores may be
used to separate digits, and a decimal point is required and must
be surrounded by digits. Therefore `0.` and `.0` are not permissible
floating literals.

IEEE NaN (Not a Number) values can be checked for with the function `isnan`.
IEEE Infinity values can be checked for with the function `isinf`.


Approximate Complex
-------------------

Felix has three complex types based on C++ complex<T> where
T is one of the three floating types.


==========  ================================
Felix       C++               
==========  ================================
fcomplex    ::std::complex<float>
dcomplex    ::std::complex<double>
lcomplex    ::std::complex<long double>
==========  ================================

Constructors
~~~~~~~~~~~~

Complex numbers can be constructed using the
typedef name `complex` and two arguments of the same
floating real type, or the typedef name `polar` and
two arguments of the same floating real type. These
construct a complex number in Cartesian and Polar
coordinates, respectively.


Approximate Quaternion
----------------------

Felix provides a single type `quaternion` which is based on
float double. It is defined in the library module `Quaternion`
which should be opened to use it.


