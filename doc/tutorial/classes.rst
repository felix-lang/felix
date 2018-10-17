Classes
=======

In Felix a class is used to provide a space for defining
functions and procedures.

.. code-block:: felix

  class X {

    // quadratic ax^2 + bx + c, solution:
    fun qplus (a:double, b:double, c:double) =>
      (-b + sqrt (sqr b + 4.0 * a * c)) / ( 2.0 * a) 
    ;

    // square it
    private fun sqr(x:double) => x * x;
  }

Notice the `qplus` function can call the `sqr` function even though it
is defined later. Felix uses random access, or setwise, lookup,
not linear lookup.
 
In order to use a function in a class, we can use explicit qualification:

.. code-block:: felix

  println$ X::qplus(1.0, 2.0, 3.0);

Alternatively, we can open the class:

.. code-block:: felix

  open X;
  show (qplus 42);

However we cannot access the function `sqr`, because it is private to the class.

A class definition must be contained in a single file, it cannot be extended.



