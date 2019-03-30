Generalised Algebraic Datatypes
===============================

A Generalised Algebraic Datatype, or GADT, is an extension of the
basic variant concept:

.. code-block:: felix

  variant pair[T] =
  | PUnit of unit => pair[unit]
  | PInt[T] of int * pair[T] => pair[int * pair[T]]
  | PFloat[T] of float * pair[T] => pair[float * pair[T]]
  | PString[T] of string * pair[T] => pair[string * pair[T]]
  ;


This looks like an ordinary variant except there is an extra term
on the RHS which is always the variant with some subscript.

With an ordinary variant of one type variable `T` the RHS
constructor is always the variant type, in this case `pair`
with its universal quantifiers, in this case type variable `T`.

With a GADT the subscript can be an arbitrary type function
of the type variables instead of just `T`.

Given the above GADT here are some values:

.. code-block:: felix

  var x1 : pair[unit] = #PUnit[unit];
  var x2 : pair[int * pair[unit]] = PInt (1,x1);
  var x3 = PFloat (42.33f, x2);


To use a GADT you need to write a generic function:

.. code-block:: felix

  fun show [W:GENERIC] (x:W):string=
  {
   match x with
   | PUnit => return "PUnit";
   | PInt (head, tail) => return "PInt(" + head.str+", " + tail.show+ ")";
   | PString (head, tail) => return "PString(" + head+", " + tail.show+ ")";
   | PFloat (head, tail) => return "PFloat(" + head.str+", " + tail.show+ ")";
   endmatch;
  }

  println$ "x3=" + x3.show;

The reason for the generic function is that it provide static polymorphic
recursion.

GADTs with existentials
-----------------------

A GAD constructor can introduce an extra type variable
called an existential variable:

.. code-block:: felix

  variant pair[T] =
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

The advantage of this `pair` over the previous one is that it works
for *any* type U, not just int, string or float. This GADT is actually
defining a tuple recursively.

The function which analyses the GADT must be generic since the
decoder requires polymorphic recursion. Note in particular the
type of `b` on which `f` is called is not the same as `x`.

Another Example
---------------

This example is from Wikipedia:

.. code-block:: felix

  variant Expr[T] =
    | EBool of bool => Expr[bool]
    | EInt of int => Expr[int]
    | EEqual of Expr[int] * Expr[int] => Expr[bool]
  ;

  fun eval(e) => match e with
    | EBool a => a
    | EInt a => a
    | EEqual (a,b) => eval a == eval b
    endmatch
  ;

  var expr1 = EEqual (EInt 2, EInt 3);
  println$ eval expr1;

In this example we have boolean and integer values and an equality
operator. The important thing is that equality only works on
integers and returns a bool: without GADTs there is no type safe
way to express this constraint.







