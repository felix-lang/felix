Typecase
=========

A typecase expression computes a value based on a type match:

.. code-block:: felix

  fun f[T:GENERIC] (x:T) =>
    typecase T with
    | int => "int"
    | double => 4.2
    | _ => "notnum"
    endmatch
  ;

  println$ f 1;
  println$ f 3.4;
  println$ f "bad";

Type case expressions are only useful in generic functions.

Typeof operator
===============

Just as typecase maps from types to values (more precisely
type expressions to value expressions), the typeof operator
does the opposite: it maps from expressions to types.
For example:

.. code-block:: felix

  fun f(x) = {
    var y : typeof x; 
    y = x;
    return x,y;
  }
  println$ f "hello";
  println$ f 42;

