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
-------------------------

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
-----------------------------

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
----------------------------

Like tuples anonymous sums support covariant depth subtyping
but not width subtyping, for the same reason. It would be confusing
if a function with a bool parameter accepted a unit argument,
even though in principle 1 is a subtype of 2.

Function subtyping
------------------

Functions support subtyping, the domain is contravariant and
the codomain is covariant.

Abstract Pointer Subtyping
--------------------------

Abstract pointers are defined by:

.. code-block:: felix

  typedef fun rptr (T:TYPE) : TYPE =>  (get: 1 -> T);
  typedef fun wptr (T:TYPE) : TYPE =>  (set : T -> 0);
  typedef fun rwptr (T:TYPE) : TYPE => (get: 1 -> T, set : T -> 0);

and therefore as record types follow the subtyping rules
for records. In particular, the read-write pointer type `rwptr` is
a subtype of both the read-only pointer `rptr` and write-only pointer
`wptr` by record with subtyping rules.

`rptr` is covariant by a combination of
depth subtyping rules for records and the covariance of function codomains.

`wptr` is contravariant by a combination of depth subtyping rules
for records and contravariance of function domains.

`rwptr` is invariant since it must be simultaneously covariant and
contravariant.

Machine Pointer Subtyping
-------------------------

Felix has three primary machine pointer types, a read-only pointer,
a write-only pointer, and a read-write pointer, which is a subtype
of the other two types. The type pointed at is invariant.

Although in principle, machine pointers should follow the model
for abstract pointers, depth subtyping is not supported because it
cannot be concretely implemented in general.

There is however one special exception. In principle, variance
can be implemented if the coercion is phantom, that is, it impacts
the type system but does not change the underlying machine address.
Felix considers a write-only machine pointer to a `uniq T` to be a 
subtype of a write-only pointer to a `T`. This is necessary so
that the procedure:

.. code-block:: felix

  proc storeat[T] ( p: &>T, v: T) = { _storeat (p,v); }

works with pointer to a `uniq T`. Without this rule,
assignments to uniquely type variable would not be possible,
such assignments actually model moves.

Unique Subtypes
---------------

The type `uniq T` is a subtype of `T`. This means a parameter of type T
can be passed an argument of type `uniq T`, discarding the uniqueness.
This works because the compiler back end discards the uniqueness constructor
anyhow, so the binding degenerates to a non-unique operation after
unique typing rules are validated.



Subtyping with Nominal Types
----------------------------

Felix allows monomorphic nominal types to form subtyping
relations by allowing the user to define coercions.


Signed Integers
+++++++++++++++

For example Felix provides:

.. code-block:: felix

  supertype vlong: long = "(long long)$1";
  supertype long: int = "(long)$1";
  supertype int : short= "(int)$1";
  supertype short : tiny = "(short)$1";

Such subtyping coercions are transitive, for example,
`int` is a subtype of `long`. Felix will generate a composite
coercion automatically. If there is more than one composition,
the composites must have the same effect because Felix will chose
one arbitrarily.

Consider the following functions:

.. code-block:: felix

  fun add(x:int, y:int):int => x + y;
  fun add(x:long, y:long):int => x + y;
  fun add(x:vlong, y:vlong):int => x + y;

together with the subtyping rules above, the functions will behave
the same way as addition in C; that is, as if C integral promotion rules applied.

Exact Signed Integers
+++++++++++++++++++++

Felix provides subtyping for normal C signed integer types and 
exact C signed integer types.

.. code-block:: felix

  supertype int64: int32 = "(int64_t)$1";
  supertype int32 : int16 = "(nt32_t)$1";
  supertype int16 : int8  "(int16_t)$1";

however there are no conversions between signed and unsigned types,
between normal and exact types, or between unsigned types.

Real to Complex
+++++++++++++++

There are no conversions between floating point reals.
C defined promotions from float to double and double to 
long double but in fact these are wrong: numerical analysis
suggests the safe conversions actually go in the other
direction.

However float reals can be embedded in complex numbers
of the same precision:

.. code-block:: felix

  supertype fcomplex: float = "::std::complex<float>($1,0.0f)";
  supertype dcomplex: double = "::std::complex<double>($1)";
  supertype lcomplex: ldouble = "::std::complex<long double>($1)";

Note this means any complex parameter will accept a float real
argument.

Annoyance
+++++++++

Its very annoying we cannot embed integers into the floats implicitly.
However for this to work, the would have to go to `double` alone.
To get the embedding to all floating types would require subtyping
the floating types to avoid ambiguity.





