Slices
======

Felix provides two slice types, these are derived types defined 
in the library.

Basic Type
----------

A basic slice type:

.. code-block:: felix

  union slice[T] =
    | Slice_all
    | Slice_from of T
    | Slice_from_counted of T * T /* second arg is count */
    | Slice_to_incl of T
    | Slice_to_excl of T
    | Slice_range_incl of T * T
    | Slice_range_excl of T * T
    | Slice_one of T
    | Slice_none
  ;

Slice Expression
----------------

Slice values can be written like:

.. code-block:: felix

  ..                    // all
  first ..              // first to maxval
  first .+ count        // first to first+count exclusive
  first .. last         // inclusive range
  first ..< past        // range exclusing end
  Slice_one val         // single value
  Slice_none            // empty slice

The base type of a slice must be of class Integral, both signed
and unsigned types can be used. If there are two parameters
they must be the same type.

Slice Membership
----------------

A value can be tested to see if it is in a slice:

.. code-block:: felix

  42 in 1..100


   
Slice iterator
--------------

Slices are equipped with iterators. This means you can write
a loop like:

.. code-block:: felix

  for v in first .. last 
    perform println$ v
  ;

If the slice is given with literal integer values the loop is
optimised to bypass the iterator closure and use inline 
range checks.


