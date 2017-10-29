Unions
======

Unions are a nominal type which is used to specified alternatives.

.. code-block:: felix

  union Maybe[T] {
  | Nothing
  | Just of T
  }

  fun show[T] (x:Maybe[T]) => 
    match x with
    | Nothing => "Nothing"
    | Some v => v.str
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
  
