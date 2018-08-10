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

* The syntax () specifies a canonical unit tuple, a product with no components. 
* There is no tuple with one component. 
* The right associative operator `,,` prepends a value
  to an existing tuple, a heterogenous version of list cons. 
* Finally the left associative `<,,>` operator appends a value to an existing tuple.

.. code-block:: felix

  var x = (1,2,3,4);
  var y = (1,,2,,3,4);
  var z = (1,2<,,>3<,,>4);

  println$ x;
  println$ y;
  println$ z;

  var q = 42,,x;

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

For more details please see :ref:`compactlineartypes`.





