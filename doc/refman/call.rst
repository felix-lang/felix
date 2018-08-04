Subroutine Calls
================

Call
----

The `call` statement invokes a procedure:

.. code-block:: felix

  call f x;
  call g ();
  g ();
  g;
  #g;
  call h.1 x;


The word `call` can be elided. If the procedure has a unit
argument, it can be elided.


Return
------

A plain `return` returns from a procedure.
An implicit return is added to the end of a procedure.

Return from
-----------

A `return from` can be used to exit an outer procedure.

.. code-block:: felix

   proc f () {
     proc g() {
       if c do
         return;
       else
         return from f;
       done
     }
     f();
   }

Jump
----

A `jump` is a tail call. It is equivalent to a call followed
by a return.

.. code-block:: felix

  proc w() { println$ " World"; }
  proc hw () { println$ "Hello"; jump w(); }

Yield
-----

The `yield` statements returns a value whilst saving the current
program counter in a hidden local variable so that a subsequent invocation of a 
generator restarts just after the yield. For this to work a closure of a generator
must be stored and used for applications, for example in a variable.

If a generator contains a `yield` statement it is called a `yielding generator`.
Iterators are usually yielding generators.

.. code-block:: felix

  gen iterator[T] (var tail: list[T]) () = {
  again :>
    match tail with
    | Cons (hd, tl) => 
      yield Some head;
      tail = tl;
      goto again;
    | Empty => return None[T];
    endmatch;
  }

  var lst = ([ 1,2,3 ]);
  var it = iterator lst;
  var elt = it ();
  again:>
    match elt with 
    | Some => 
      println$ v;
      goto again;
    | None => ;
    endmatch;



