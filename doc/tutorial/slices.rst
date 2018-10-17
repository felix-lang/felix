Slices
======

A slice is a range of integers.

Type
----

The type is defined in the library as

.. code-block:: felix

  slice[int]
 

Inclusive Slice
---------------

From first to last, inclusive:

.. code-block:: felix

  first..last

Exclusive Slice
---------------

From first to last, exclusing last:

.. code-block:: felix

  first..<last

Counted Slice
-------------

From first for a certain number of values:

.. code-block:: felix

  first.+count

Infinite Slices
--------------

Felix provides three apparently infinite slices, which are actually
bounded by the limits of the integer type:

.. code-block:: felix

  first..     // first..maxval[int]
  ..last      // minval[int]..last
  ..<last     // minval[int]..<last

There is also a slice over the whole range of a type:

.. code-block:: felix

  Slice_all[int]
  ..[int]

Empty Slice
-----------

There is an empty slice too:

.. code-block:: felix

  Slice_none[int]


More Slices
-----------

More detail on slices in the reference manual:

https://felix.readthedocs.io/en/latest/slices.html
