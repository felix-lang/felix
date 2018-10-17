Option Type
===========

Another useful type that requires pattern matching is the polymorphic
option type `opt`. It can be used to capture a value, or specify there
is none:

.. code-block:: felix

  fun divide (x:int, y:int) =>
    if y == 0 then None[int]
    else Some (x/y)
    endif
  ;

Pattern matching optional values
--------------------------------

This is done like:

.. code-block:: felix
  
  printopt (x: opt[int]) {
    match x with
    | Some v => println$ "Result is " + v.str;
    | None => println$ "No result";
    endmatch;
  }


