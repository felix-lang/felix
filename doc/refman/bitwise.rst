Bitwise Operators
=================

Syntax
------

.. code-block:: felix

  x[sbor_pri] := x[sbor_pri] "\|" x[>sbor_pri] 
  x[sbxor_pri] := x[sbxor_pri] "\^" x[>sbxor_pri] 
  x[sband_pri] := x[sband_pri] "\&" x[>sband_pri] 
  x[sshift_pri] := x[sshift_pri] "<<" x[>sshift_pri]
  x[sshift_pri] := x[sshift_pri] ">>" x[>sshift_pri] 
  x[sprefixed_pri] := "~" x[spower_pri]


Description
-----------

The usual C operators spelled differently.

==================== ===================== ========
operator             numeric semantics     types
==================== ===================== ========
\\|                  bitwise or            uints
\\^                  bitwise exclusive or  uints
\\&                  bitwise and           uints
<<                   left shift            ints
>>                   right shift           ints
~                    ones complement       ints
==================== ===================== ========

Note that the shift operators work on signed ints because they're
defined as multiplication and division by powers of 2. All the
operations are defined mathematically and are independent of
the representation. Operations on unsigned ints work "as if"
the standard positional binary representation were used.

