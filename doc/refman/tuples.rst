Tuples
======

Syntax
^^^^^^


  //$ Tuple type, non associative
  x[sproduct_pri] := x[>sproduct_pri] ("*" x[>sproduct_pri])+ 

  //$ Tuple type, right associative
  x[spower_pri] := x[ssuperscript_pri] "**" x[sprefixed_pri]

  //$ Tuple type, left associative
  x[spower_pri] := x[ssuperscript_pri] "<**>" x[sprefixed_pri]

  //$ Array type
  x[ssuperscript_pri] := x[ssuperscript_pri] "^" x[srefr_pri]

  //$ Tuple pattern match right associative
  stuple_pattern := scoercive_pattern ("," scoercive_pattern )*

  //$ Tuple pattern match non-associative
  stuple_cons_pattern := stuple_pattern ",," stuple_cons_pattern

  //$ Tuple pattern match left associative
  stuple_cons_pattern := stuple_pattern "<,,>" stuple_cons_pattern 

  //$ Tuple projection function.
  x[scase_literal_pri] := "proj" sinteger "of" x[ssum_pri]


Constructors
^^^^^^^^^^^^

.. code-block:: felix

  // unit tuple constructor
  satom := "(" ")" 

  //$ Tuple formation non-associative.
  x[stuple_pri] := x[>stuple_pri] ( "," x[>stuple_pri])+ 

  //$ Tuple formation by prepend: right associative
  x[stuple_cons_pri] := x[>stuple_cons_pri] ",," x[stuple_cons_pri]

  //$ Tuple formation by append: left associative
  x[stuple_cons_pri] := x[stuple_cons_pri] "<,,>" x[>stuple_cons_pri]


There are four basic constructors. The syntax () specifies a canonical 
unit tuple, a product with no components. There is no tuple with
one component. The right associative operator `,,`' prepends a value
to an existing tuple, a heterogenous version of list cons. Finally
the left associative `<,,>` operator appends a value to an existing tuple.


No Tuples of one component
~~~~~~~~~~~~~~~~~~~~~~~~~~

There are no tuples of one component.


Recursive Formulations
~~~~~~~~~~~~~~~~~~~~~~

Tuples also have a recursive formulation for values
using a right associative binary constructor:

.. code-block:: felix

   var x = 1,,"hello",,42.3,77;

Note carefully that the right most constructor above is a single comma ","!
The double comma ",," constructor right argument must be a tuple.
Nor will adding a unit tuple "() work!

.. code-block:: felix

   var x = 1,,"hello",,42.3,,77; // Fails, 77 not tuple
   var x = 1,,"hello",,42.3,,77,,(); // Fails, 77,,() = 77 not tuple

There is a corresponding way to specify types:

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

There is also a left associative binary constructor:


.. code-block:: felix

   var x = 1,"hello"<,,>42.3<,,>77;

with a corresponding type:

.. code-block:: felix

   typedef tup = 1 * string ** double ** int;


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





