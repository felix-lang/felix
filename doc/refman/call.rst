Subroutine Calls
================

Call
----

The call statement invokes a procedure:

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

A jump is a tail call. It is equivalent to a call followed
by a return.

.. code-block:: felix

  proc w() { println$ " World"; }
  proc hw () { println$ "Hello"; jump w(); }




