Subtyping Rules
===============

Subtyping rules specify when Felix can perform implicit coercions.

Records
-------

Records support both width and covariant depth subtyping.

Width subtyping
+++++++++++++++

A parameter accepting a certain set of fields can be passed
a record value with additional fields. Felix inserts a coercion
which builds a new record with only the required fields:


.. code-block:: felix

  fun f(p : (a:int, b: int)) => p.a + p.b;
  f (a=1,b=2,c=3); // OK, c field is discarded

Depth subtyping
+++++++++++++++

A parameter accepting a record with a field of type P
will accept a record with the field of type B provided
B is a subtype of P:

.. code-block:: felix

  fun f(p : (a=int, b=(c:int, d:int))) => p.a + p.b.c + p.b.d;
  f (a=1,b=(c=2,c=3,d=4)); // OK, b value is coerced

In this example, the type of field `b` of the argument 
is coerced to the type of field `b` of the parameter,
as it happens by width subtyping.

Depth and width subtyping can be used simulaneously and
apply recursively.

Tuple and array subtyping
+++++++++++++++++++++++++

Tuples, and thus arrays, support covariant depth subtyping.
For example:

.. code-block:: felix

  f(p: (a:int,b:int), d:int) => p.a + p.b +p.c;
  println$ f ((a=1,b=2,c=3,42); // OK, coerce first component

Although well principled, width subtyping is not supported
for tuples and arrays, even though this is a special case
of record subtyping. This is because it is likely to be
confusing in the presence of overloading.

Polymorphic Variant Subtyping
+++++++++++++++++++++++++++++

Polymorphic variants support width and covariant depth
subtyping, however, the width subtyping rule is the
reverse of that for records, the argument must have less fields:

.. code-block:: felix

  typedef psub = (`A | `B | `C);
  typedef psup = (`A | `B | `C | `D);
  var v = `A :>> psub;
  fun f(p: psup) => match p with
    | `A => "A"
    | `B => "B"
    | `C => "C"
    | `RD=> "D"
    endmatch
  ;
  println$ f v;
 
   
The function `f` can handle 4 cases, so passing an argument which could
only be one of three of them is safe.

Anonymous Sum Type Subtyping
++++++++++++++++++++++++++++

Like tuples anonymous sums support covariant depth subtyping
but not width subtyping, for the same reason. It would be confusing
if a function with a bool parameter accepted a unit argument,
even though in principle 1 is a subtype of 2.

Function subtyping
++++++++++++++++++

Functions support subtyping, the domain is contravariant and
the codomain is covariant.


