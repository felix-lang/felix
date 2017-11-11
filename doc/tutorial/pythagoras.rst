Pythagoras
==========

The ancient Greek mathematician Pythagoras is famous for
the invariant of right angle triangles:

.. math::

    h ^ 2 = w^ 2 + a^2

where `h` is the hypotenuse, `w` is the width of the base,
and `a` is the altitute.

We can calculate the hypotenuse in Felix like this:

.. code-block:: felix

    fun hypot (w:double, a:double) : double =>
      sqrt (w^2 + a^2)
    ;

    println$ hypot (3.0, 4.0);

The type `double` is a standard double precision floating point real
number. 

The `sqrt` function is in the library, and calculates the
square root of a double precision number.

The operator `^` denotes exponentiation, in this case
we are squaring, or muliplying the argument by itself twice,
the literal `2` is a value of type `int`, a type of small
integers.

Of course, the operator `+` is addition.

The `fun` binder is used here to define a function. Then we give
the function name we want to use, in this case `hypot`.

Then, in paranthesis we give a comma separated list of paramater
specifications. Each specification is the name of the parameter,
followed by its type.

It is good practice, but not required, to follow the parameters
with `:` and the return type of the function. 

Then the `=>` symbol is used to begin the formula defining the function
in terms of the parameters.

The function can be used by applying it to an argument of the 
correct type,  in this case a pair, or tuple, of two numbers
of type `double`. 

The `println` is then called on the application using
the application operator `$`.


