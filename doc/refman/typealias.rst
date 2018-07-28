Type Aliases
============

Simple Alias
------------

Felix allows you to define an alias for a type:

.. code-block:: felix

  typedef myint = int;

Type Schema
-----------

An alias can be polymorphic, in which case it specifies
a type schema:

.. code-block:: felix

  typedef pair[T,U] = T * U;
  var x: pair[int, double] = 42, 7.2;


