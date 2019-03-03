Struct Constructors
===================

Sometimes, you want to define extra constructors for a struct,
you can do this with a `ctor` function.

.. code-block:: felix

  struct Point {
    x: int;
    y: int;
  }
  ctor Point (a:int) => Point (a,a);
  var p = Point 42;

In fact, this works for any type, not just a struct, provided the
type has a single word name. If it doesn't, you can always introduce
a `typedef`:

.. code-block:: felix

  typedef pair = int * int;
  ctor pair (x:int) => x,x;

Using a `typedef` is a way to introduce an extra constructor for a type
with the same signature; that is, provide *named constructors*. For example:

.. code-block:: felix

  struct complex { 
    x: double;
    y: double;
  }

  typedef polar = complex;
  ctor polar (modulus: double, argument: double) =>
    complex (modulus * cos argument, modulus * sin argument)
  ;



   
