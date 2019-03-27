Compact Linear Types
====================

A compact linear type either the unit type 1, the void type 0, or any small
sum or product of compact linear types. Small here means the representation
as described below fits into a 64 bit machine word.

Compact linear types have two primary uses: for sub word level value manipulation
and to support polyadic array operations.

 
Unitsums
--------

The following types are `unitsum` types:

.. code-block:: felix

   typedef void = 0;
   typedef unit = 1;
   typedef bool = 2; // 1 + 1
   4 // 1 + 1 + 1 + 1
   5 // 1 + 1 + 1 + 1 + 1
   ...

These are sums of 0, 1, 2, 3, 4, 5 .. units and represent 0, 1, 2, 3,
4, 5 alternatives. Values of unitsum types have two notations:

.. code-block:: felix

  case 3 of 5
  `3:5

The first syntax is general, the second is peculiar to unitsums.
Note that there are no values of type void, because there are no
alternatives. The value of type unit can also be written:

.. code-block:: felix

   ()

and represents exactly one alternative. Values of type bool can also be
written

.. code-block:: felix

   false // `0:2
   true // `1:2


Note that somewhat unfortunately, the value index is zero origin
so `case 0 of 5` is the first of 5 cases and `case 4 of 5` is 
the last. It reads badly but zero origin was chosen for symmetry
with C array indexing conventions.

All unitsums from 1 up are represented by 64 bit unsigned integers.
[This may be relaxed and/or extended in future versions of Felix]

Products
--------

Products of compact linear types are compact linear types. For example:

.. code-block:: felix

  var x : 2 ^ 4 = true, false, false, true;
  println$ x.1; // false

This is an array of 4 bits, but it has a magic property: it is compact linear
so it is represented by a single machine word. Arrays of up to 64 bits 
are represented by single machine words. Here is another example:

.. code-block:: felix

  var x : 2 * (3 * 4) = true, (`1:3, `2:4);
  println$ x.1.1._strr; // case 1 of 3

Again, the value is a single machine word. Compact linear types
are similar to C bit fields, however a C bitfield must consist of
$n$ bits and so represents $2^n$ values. Felix compact linear
types have no such constraint.

Compact linear types are represented by standard variable radix
number system. The easiest explanation is the following example:

.. code-block:: felix

   var time = (`2:24, `30:60, `1:60); // 1 second past 2:30 am
   var secs = time :>> int;
   println$ "Seconds into day = " + secs.str;
   assert secs == 60 * 60 * 2 + 60 * 30 + 1;

Compact linear types are named that because if there are $n$ possible
value they're represented by a range of integers from 0 upto $n-1$.
This range is compact, meaning there are no holes in it, and linear,
because it is integral.





