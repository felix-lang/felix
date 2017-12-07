Polymorphic Functions
=====================

Felix allows function to be polymorphic.
This means you can write a function that works, in part, for 
any type.

.. code-block:: felix

    fun swap[A,B] (x:A, y:B) : B * A => y,x;

This is called parametric polymorphism. The names
`A` and `B` are called type variables. The above
fuction will work for any actual types:

.. code-block:: felix

    println$ swap[int,string] (42, "Hello");

Here, the specific types used we given explicitly.
This is not required if the types can be deduced
from the arguments of the application:

.. code-block:: felix

    println$ swap(42, "Hello");

Here, `A` must be `int` because parameter x has type `A`,
and the argument 42 has type `int`. Similarly, `B` must
be `string` because "hello" has type string.

