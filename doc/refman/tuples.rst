Tuples
======

In Felix a tuple value may be specified with the n-ary infix comma operator:

.. code-block:: felix
 
   var x = 1, "Hello", 42.3;
   println$ x;

The type of `x` above is given by 

.. code-block:: felix

    typedef tup_t = int * string * double;


Tuples are the usual cartesian product. The comma operator
is not associative, and it is not a binary operator. The following
tuples are all distinct and have distinct types as indicated:

.. code-block:: felix

    var a : int * string * double = 1,"Hello",42.3;
    var b : (int * string) * double = (1,"Hello"),42.3;
    var a : int * (string * double) = 1,("Hello",42.3);

Unit Tuple
~~~~~~~~~~

There is a special tuple with no components known as the unit value
or empty tuple, and written:

.. code-block:: felix

      var x : 1 = ();

It has the type 1 as indicated, which has the alias

.. code-block:: felix

    typedef unit = 1;

which is defined in the library.

No Tuples of one component
~~~~~~~~~~~~~~~~~~~~~~~~~~

There are no tuples of one component. More precisely, every value
may be consider as a tuple of one component.

Field Access
~~~~~~~~~~~~

Tuple fields are positional and accessed using a plain decimal integer literal:

.. code-block:: felix

   var x = 1, "Hello", 42.3;
   println$ x.1; // hello
   println$ 1 x; // hello

The number is 0 origin. A standalone projection can be created like this:

.. code-block:: felix

   var x = 1, "Hello", 42.3;
   var prj = proj 1 of (int * "Hello" * double);
   println$ prj x; // hello


Recursive Formulation
~~~~~~~~~~~~~~~~~~~~~

Tuples also have a recursive formulation for values:

.. code-block:: felix

   var x = 1,,"hello",,42.3,77;

and for types:

.. code-block:: felix

   typedef tup = 1 ** string ** double * int;

This is an alternate syntax in which a tuple is
treated like a heterogenous list, the values
contructed are identical to those using the n-ary
formulation. The recursive format is useful for pattern matching
associated with GADTs or type classes with polymorphic recursion.

The right hand side of a `,,` value constructor or `**` type constructor
must be a tuple of at least 2 components.

The recursive formulation requires the right argument of the operator
to be a tuple. The unit tuple can be used so that

.. code-block:: felix
 
   1,2 == 1,,2,,()

Note that

.. code-block:: felix

  42,,() = 42

because a tuple with one component is identical to that component in Felix.

Arrays
~~~~~~

If all the components of a tuple have the same type, then the 
tuple is called an array. An alternate more compact type
annotation is available for arrays:

.. code-block:: felix

   var x : int ^ 4 = 1,2,3,4;

In addition, arrays allow an expression for projections, as well
as decimal integer literals. Two types may be used for an array
index:

.. code-block:: felix

   var x : int ^ 4 = 1,2,3,4;
   for i in 0..<4 perform println$ x.i;
   println$ x.(`1:4);

The index of an array, in this case `4` is not an integer, it is a 
sum of 4 units, representing 4 cases. Therefore the correct projection
should be of type 4, however Felix allows an integral type, which is
coerced to type 4.

See the section on `sum types` for more information on unit sums.





