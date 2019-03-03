Using Classes
=============

Once we defined a class and provided some instances, we can use the
class for those instances. There are several ways to do this.

Qualified names
+++++++++++++++

You can use a fully qualified name to access a class method:

.. code-block:: felix

  println$ Eq[int]::==(1, 2);
  println$ Eq[int]::eq(1, 2);
  println$ Eq[double]::eq(1.1, 2.2);

You can leave out the type when it can be deduced by overload
resolution:

.. code-block:: felix

  println$ Eq::==(1, 2);
  println$ Eq::eq(1, 2);

Missing Instances
+++++++++++++++++

If you try to use a method which has not been defined by an instance
for the type you used, you will get an *instantiation error*. This is not
a type error, it just means you forgot to provide an instance for
that type:

.. code-block:: felix

  // println$ Eq::==("Hello", "World");
  // WOOPS! we didn't defined it for strings


Opening a Class
+++++++++++++++

The easiest way to use a class is to open it:

.. code-block:: felix

  open Eq;
  println$ eq(1,2);
  println$ 1 == 2;
  println$ 1.1 == 2.2;

This is the most common method for script. In fact many classes
in the standard library have already been opened for you.

Opening a Class Specialisation
++++++++++++++++++++++++++++++

It is also possible to open a specialisation of a class:

.. code-block:: felix

  open Eq[int];
  println$ eq(x,y);
  println$ x == y;
  // println$ 1.1 == 2.2;
  // WOOPS, can't find == for double * double

This is not an instantiation error, and instance for double
exists. The problem is that we didn't open Eq for double,
only for int.

Passing a class to a function
+++++++++++++++++++++++++++++

You can also pass a class or specialisation to a function.
Here is a monomorphic example:

.. code-block:: felix

  fun eq3 (x:int, y:int, z:int with Eq[int]) =>
    x == y and y == z
  ;


This is exactly the same as opening the class or specialisation
inside the function. It is useful because it isolates the access
to the class by functional abstraction.

Here is a polymorphic example:

.. code-block:: felix

  fun eq3[T] (x:T, y:T, z:T with Eq[T]) =>
    x == y and y == z
  ;


