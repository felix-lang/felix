Unions
======

Basic Unions
------------

Unions are a nominal type which is used to specified alternatives.

.. code-block:: felix

  union Maybe[T] {
  | Nothing
  | Just of T
  }

  fun show[T] (x:Maybe[T]) => 
    match x with
    | Nothing => "Nothing"
    | Just v => v.str
    endmatch
  ;

  var x = Just 1;
  println$ show x;

The fields of a union are usually called constructors. Constructors
may be either constant constructors like Nothing above, or
non-constant constructors like Just, which take an argument.

Pattern matching is used as shown to decode a value of union type.
Matches consist of an argument and a list of branches. 
Each branch contains a pattern and a handler. 

In the Some branch the pattern variable v is set to 1 in the example,
and converted to a string in the handler expression.

Pattern matching selects the first branch with a pattern that matches
and evaluates the corresponding handler.
 

Generalised Algebraic Data Types
-------------------------------- 

Felix also provides generalised algebraic data types, or GADTS:
A GADT is a polymorphc union with a per constructor existential
constraint on the type variable.

.. code-block:: felix

  union pair[T] =
  | PUnit of unit => pair[unit]
  | Pair[T,U] of U * pair[T] => pair[U * pair[T]]
  ;

  var x1 = #PUnit[unit];
  var x2 = Pair (22,x1);
  var x3 = Pair (99.76,x2);

  fun f[T:GENERIC] (x:T) = {
    match x with
    | Pair (a,b) => return a.str + ","+b.f;
    | PUnit => return "UNIT";
    endmatch;
  }

  println$ f x3;

With a GADT, some components may have a RHS after the `=>` symbol which must be
the union type subscripted with a constraint on the type variables: in the
example the `PUnit` constructor returns a `pair` with parameter T constrained to type unit,
whereas the `Pair` constructor introduces and existential parameter T,
and returns a `pair` with type argument `pair[U * pair[T]]`.

This particular construction can be used to recursively define a heterogenous
list similar to a system tuple type, but amenable to recursive analysis
by a generic function such as `f` above. The analysis requires polymorphic
recursion which Felix does not support directly but in this case can
be emulated by a generic function which is expanded by the compiler
to a nest of specialised functions.







