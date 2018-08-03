Lists
=====

.. code-block:: felix

  syntax listexpr
  {
    //$ List cons, right associative.
    x[sarrow_pri] := x[>sarrow_pri] "!" x[sarrow_pri]

    satom := "(" "[" stypeexpr_comma_list "]" ")" 
  }

Semantics
---------

.. code-block:: felix

  union list[T] = | Empty | Snoc of list[T] * T;

  fun _match_ctor_Cons[T] : list[T] -> bool = "!!$1"; 

  inline fun _ctor_arg_Cons[T]: list[T] -> T * list[T]

  inline fun Cons[T] (h:T, t:list[T]) => Snoc (t,h);

Lists in Felix are defined as Snoc lists, in which the order list tail
occurs *before* the node data value. This allows some operations to
be written in C++ which work for lists, independently of the data type
of the stored value T.

However lists are usually used with a Cons operator for which the
head value goes first, then the tail. This is arranged by providing
a Cons function to construct lists, and by providing user defined
pattern matches for the Cons form.

Lists in Felix are purely functional and immutable.

