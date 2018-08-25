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


There are four basic constructors. 

Unit Tuple
++++++++++

The syntax () specifies a canonical unit tuple, a product with no components. 

.. code-block:: felix

  ()


Tuple of one component
++++++++++++++++++++++

A tuple of one component is identical to that component.
The projection for that tuple is the identity function.

Chained comma
+++++++++++++

A list of expressions separated by commas forms a tuple.
The comma operator is said to be a chain operator.
The operation is non-associative.

Parentheses may be used to indicate grouping.

.. code-block:: felix

  1,"hello",42.1
  (1,"hello"), 42.1
  1,("hello", 42.1)

All three of these tuples are disinct.

Prepend operator
++++++++++++++++

The `cons` form of a tuple uses the right associative
binary operator `,,` to prepend a value to an existing
tuple of at least two components.

.. code-block:: felix

  var x = "a",,1,,"hello",42,1;
  var y = "joy",,x;;

Note that the first case must use a `,` because the right
hand term of the `,,` operator must be a tuple. Although
`()` is a tuple, `1,()` is equal to `1` and is not.


Append operator
+++++++++++++++

Finally the left associative `<,,>` operator appends a value to an existing tuple.
It is provided for symmetry with `,,` but the syntax is arcane.

.. code-block:: felix

  var x = 1,"hello"<,,>42.4;
  var y = x<,,>"bye";

The left term of the append operator must be a tuple with at least two
components.

Addition operator
+++++++++++++++++

The left associative infix binary addition operator `+` can also be used
to append a value to a tuple. It has the same semantics as the `<,,>`
operator but a different precedence. Its use can be confusing
sometimes, as it can be mistaken of integer addition.

.. code-block:: felix

  var x = "Hello",1;

  println$ x + x; // ("Hello", 1, ("Hello", 1))
  println$ x + 1; // ("Hello", 1, 1)
  println$ 1 + x; // (1, ("Hello", 1))

Extend operation
++++++++++++++++

The `extend..with..end` operator packs a list
of tuples or values into a tuple. The initial values are
separated by commas, then a `with` keyword is used, then
the remaining values are given, terminated by the `end` keyword:

.. code-block:: felix

  var y = extend (1,2), "hello", (42.2,("bye",99)) with (55,"hh") end;
  // (1, 2, hello, 42.2, (bye, 99), 55, hh) 

  var z = "hello",22;
  println$ extend z with 34 end;
  // ("hello", 22, 34) 

Note that extend is expanded before monomorphisation, so a type
variable will be treated as a single value, even if it is later
replaced by a pair: had a pair been given both values would
be in the result, instead of a single pair.

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


Value Projections
^^^^^^^^^^^^^^^^^

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
  println$ x.p; // "hello"

Pointer Projections
^^^^^^^^^^^^^^^^^^^

For every value projection, there is a corresponding pointer
projection, represented as an overloaded function.
That is, you can use the same syntax for projections on pointers
as values.

.. code-block:: felix

  var x = 1,"hello",42;
  var p = proj 1 of &(int * string * int);
  println$ *(p &x); // "hello"
  println$ *(&x.p); // "hello"
  println$ *(x&.p); // "hello"
  var wop = proj 1 of &>(int * string * int);
  &>x . wop <- "bye";
  var rop = proj 1 of &<(int * string * int);
  println$ *(&<x . rop);

Note the special sugar `x&.p` which is equivalent to `&x.p`.

It's important to note that the application of projections to pointers
as well as values *solves a major problem in C++* by eliminating
entirely any need for the concept of lvalues and reference types.
Pointers are first class values and the calculus illustrated above
forms a coherent algebra which cleanly distinguishes purely
functional values, but, via pointers, provides the same algebraic
model for imperative code.

The concept of a pointer cleanly distinguishes a value from
a mutable object. In particular

* all values are immutable, but
* all products are mutable
  and their components separately mutable, if you can obtain
  a pointer to the value type.

There are three basic ways to do this:

* store the value in a variable and takes its address, or, 
* copy the value onto the heap with
  operator `new` which returns a pointer. 
* Library functions can also provide pointers.

The fundamental calculus of projections is just ordinary functional
calculus. This is the point! In particular composition of pointer projections
is equivalent to adding the offsets of components in a nested
product. For example:

.. code-block:: felix

  var x = (1,(2,3));
  var p1o = proj 1 of (&(int * int^2)); 
  var p1i = proj 1 of (&(int^2));
  var p = p1o \odot p1i; // reverse composition
  println$ *(&x.p); // 3

The address calculations are purely functional and referentially
transparent.

Projection Applications
^^^^^^^^^^^^^^^^^^^^^^^

There is a short cut syntax for applying a projection to a tuple,
you can just apply an integer literal directly:

.. code-block:: felix

  var x = 1,"hello",42;
  println$ 0 x, x.1;

Note that since operator dot `.` just means reverse application,
then `x.1` is the same as `1 x`.

Slice Value Projections
^^^^^^^^^^^^^^^^^^^^^^^

A slice with integer literal arguments can be applied to a tuple to construct
a new tuple consisting of the components in range of the slice. The components
selected are in the intersection of the given slice and a slice from 0 to
the length of the tuple minus 1. No error is possible.

.. code-block:: felix

  var x = 1,4.2,"hello",42u;
  println$ x. Slice_all;       // (1, 4.2, hello, 42)
  println$ x. (..);            // (1, 4.2, hello, 42) 
  println$ x. (1..);           //  (4.2, hello, 42)
  println$ x. (..3);           // (1, 4.2, hello, 42)
  println$ x. (1..3);          // (4.2, hello, 42)
  println$ x. (1..<3);         // (4.2, hello)
  println$ x. (1.+2);          // (4.2, hello)
  println$ x. (3..0);          // ()
  println$ x. Slice_none;      // ()
  println$ x. (Slice_one 2);   // "hello"

Ties and Slice Pointer Projections
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Felix has a generic operator `_tie` which takes a pointer
to a structurally typed product and produces an isomorphic
product type, where the type of each component becomes
a pointer to the component type. This is called a tie:

.. code-block:: felix

  var x = 1,4.2,"hello",42u;
  var px = _tie &x; // a tuple of pointers
  println$ *(px . 2); // hello
 
The same effect can be obtained for tuples, with a pointer slice:

.. code-block:: felix

  var x = 1,4.2,"hello",42u;
  var px = &x . (..); // a tuple of pointers
  println$ *(px . 2); // hello

Of course, any slice can be used, and, it also works for read only
and write only pointers.
 
[And should if the product is a compact linear type or array,
neither of which is implemented as at 2018/08/25]

Reversed Tuple
^^^^^^^^^^^^^^

For any tuple the generic operator `_rev` forms a tuple
with the components in reversed order.

.. code-block:: felix

  var x = 1,4.2,"hello",42u;
  println$ _rev x; // (42, hello, 4.2, 1)

 

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

Tuple patterns are an advanced kind of tuple accessor.

.. code-block:: felix

  match 0,1,(2,3,(4,5,6),7,8) with
  |  _,x1,(x2,_,(x4,,x56),,x78 => 
     // x1=1, x2=2, x4=4, x56=(5,6), x78=(7,8)
     ...
  endmatch

Tuple patterns are *irrefutable*, that is, they cannot fail to match
if they type check, provided subcomponent matches are also
irrefutable. For this reason they are often used in `let` form
matches which only admit one branch syntactically:

.. code-block:: felix

  let _,x1,(x2,_,(x4,,x56),,x78 =
    0,1,(2,3,(4,5,6),7,8)
  in
     // x1=1, x2=2, x4=4, x56=(5,6), x78=(7,8)
     ...


Arrays
~~~~~~

If all the components of a tuple have the same type, then the 
tuple is called an array. Perhaps more precisely, a fixed length array
where the length is fixed at compile time. The jargon `farray` is
sometimes used to be specific about this kind of array.

An alternate more compact type annotation is available for arrays:

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

Array Projections
^^^^^^^^^^^^^^^^^

Array projections are similar to non-array projections except that
the projection index can be an expression. The keyword `aproj` must
be used for an array projection, and the indexing type must
be precisely the type of the array exponent.

.. code-block:: felix

  var x = 1,2,3,4;
  var n = `2:4; // index
  var px = aproj n of (int ^ 4);
  println$ x.px;

A direct application may also use a one of two shortcut forms:

.. code-block:: felix

  var x = 1,2,3,4;
  var n = `2:4;   // precise index
  println$ x . n; // precise shortcut projection
  var m = 2;      // integer index
  println$ x . m; // checked shortcut

The integer form requires a run time bounds check.

Generalised Arrays
^^^^^^^^^^^^^^^^^^

By virtue of the existence of compact linear types and coercions
representing isomorphisms on them, Felix supports a notion
of generalised arrays. In particular, the structure of an array
does not have to be linear.

For example:

.. code-block:: felix

  var x : (int ^ 3) ^ 2 = ((1,2,3),(4,5,6));
  var y : int ^ (2 * 3) = x :>> (int ^ (2 * 3));
  var z = x :>> (int ^ 6);
  for i in 0..<2
    for j in 0..<3 do
      println$ x.i.j;
      println$ y.(i:>>2,j:>>3);
      println z.(i * 3 + j);
    done

Note the unfortunate requirement to coerce the integer indices
to the precisely correct type. [To be fixed]

In this example, `x` is an array of arrays, however `y` 
is a *matrix*: the index of the matrix is not a single
linear value but rather, the index is a tuple. 

The coercion used to convert type `x` to `y` is an isomorphism.
Underneath in *both* cases we have a linear array of 6 elements, `z`.

The coercions on the arrays above are called `reshaping` operations.
They are casts which *reconsider*, or *reinterpret* the underlying linear array
as a different type.

Note: in the matrix form, Felix has chosen the indexing tuple
to be of type `2 * 3` so that a reverse application can be thought
of as first selecting one of the two subarrays, then selecting 
one of the three elements. In other words, you write the `i` and `j`
indices in the array of arrays form and matrix form in the same order
when using reverse application. However the order differs if you
use forward application:

.. code-block:: felix

  for i in 0..<2
    for j in 0..<3 do
      println$ j (i x);
      println$ (i:>>2,j:>>3) y;
    done

The choice of ordering is arbitrary and confused by the fact
that numbers are written in `big-endian` form which tuple
indices are written in `little-endian` form. The use of
big-endian numbers is unnatural in western culture
where script is written from left to right, we should be
using little-endian. However our number system is derived
from tha Arabic, which is written right to left, so in that
script, numbers put the least significant digits first.
   
Consequently there is a natural ordering conflict, since
our numbers are backwards from our ordering of array elements.
Take care with, for example, square matrices where the type
system cannot detect an incorrect ordering. Take evem more
care with coercions, since they override the type system!
 
Polyadic Array Handling
^^^^^^^^^^^^^^^^^^^^^^^

Because of the reshaping isomorphisms, it is possible to write
a single *rank independent* routine which performs some action
on a linear array which can be applied to an array of any shape.
All you need to do is coerce the generalised array argument to a suitable
isomorphic linear form, apply the routine, and cast the resulting
linear array back.

For more details please see :ref:`Compact Linear Type <compactlineartypes>`.





