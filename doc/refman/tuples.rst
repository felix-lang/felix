Tuples
======

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

.. code-block:: felix

  var x = (1,2,3,4);
  var y = (1,,2,,3,4);
  var z = (1,2<,,>3<,,>4);

  println$ x;
  println$ y;
  println$ z;

No Tuples of one component
++++++++++++++++++++++++++

Note that there are no tuples of one component.

Types
^^^^^

.. code-block:: felix

  //$ Tuple type, non associative
  x[sproduct_pri] := x[>sproduct_pri] ("*" x[>sproduct_pri])+ 

  //$ Tuple type, right associative
  x[spower_pri] := x[ssuperscript_pri] "**" x[sprefixed_pri]

  //$ Tuple type, left associative
  x[spower_pri] := x[ssuperscript_pri] "<**>" x[sprefixed_pri]

  //$ Array type
  x[ssuperscript_pri] := x[ssuperscript_pri] "^" x[srefr_pri]

The first three types are alternate ways to express a tuple type.
The different forms are significant with polymorphism.

For example consider the following code which 
performs a lexicographic equality test on any tuple:

.. code-block:: felix

  class Eq[T] { virtual fun == : T * T -> bool }

  instance [T,U with Eq[T], Eq[U]] Eq[T ** U] {
    fun == : (T ** U) * (T ** U) -> bool =
    | (ah ,, at) , (bh ,, bt) => ah == bh and at == bt;
    ;
  }

  instance[T,U with Eq[T],Eq[U]] Eq[T*U] {
    fun == : (T * U) * (T * U) -> bool =
    | (x1,y1),(x2,y2) => x1==x2 and y1 == y2
    ;
  }

  instance[t with Eq[T]] Eq[T*T] {
    fun == : (T * T) * (T * T) -> bool =
    | (x1,y1),(x2,y2) => x1==x2 and y1 == y2
    ;
  }

This code uses polymorphic recursion via type class virtual
function overloads to analyse a tuple like a list by using
the tuple Cons operator `**`.

There are two ground cases given, the first one checks for a pair
to terminate the recursion, the second is more specialised and
checks for a pair of the same type, that is, an array of two elements.


Tuple Patterns
^^^^^^^^^^^^^^

.. code-block:: felix

  //$ Tuple pattern match right associative
  stuple_pattern := scoercive_pattern ("," scoercive_pattern )*

  //$ Tuple pattern match non-associative
  stuple_cons_pattern := stuple_pattern ",," stuple_cons_pattern

  //$ Tuple pattern match left associative
  stuple_cons_pattern := stuple_pattern "<,,>" stuple_cons_pattern 

  //$ Tuple projection function.
  x[scase_literal_pri] := "proj" sinteger "of" x[ssum_pri]

Tuple patterns are an advanced kind of tuple destructor.

Projections
^^^^^^^^^^^

.. code-block:: felix

  x[scase_literal_pri] := "proj" sinteger "of" x[ssum_pri]

Projection functions for a given tuple type can be written.
Projections are first class functions, like any other.
The projection index must be a literal decimal integer
between 0 and n-1, inclusive, where n is the number of
components of the tuple.

.. code-block:: felix

  var x = 1,"hello",42;
  var p = proj 1 of (int * string * int);
  println$ p x; // "hello"

Projection Applications
^^^^^^^^^^^^^^^^^^^^^^^

There is a short cut syntax for applying a projection to a tuple,
you can just apply an integer literal directly:

.. code-block:: felix

  var x = 1,"hello",42;
  println$ 0 x, x.1;

Note that since operator dot `.` just means reverse application,
then `x.1` is the same as `1 x`.

Arrays
~~~~~~

If all the components of a tuple have the same type, then the 
tuple is called an array. An alternate more compact type
annotation is available for arrays:

.. code-block:: felix

   var x : int ^ 4 = 1,2,3,4;

In addition, arrays allow an expression for the shortcut form of
projections applications, as well as decimal integer literals. 
Two types may be used for an array index:

.. code-block:: felix

   var x : int ^ 4 = 1,2,3,4;
   for i in 0..<4 perform println$ x.i;
   println$ x.(`1:4);

The index of an array, in this case `4` is not an integer, it is a 
sum of 4 units, representing 4 cases. Therefore the correct projection
should be of type 4, however Felix allows an integral type, which is
coerced to type 4.

See the section on `sum types` for more information on unit sums.





