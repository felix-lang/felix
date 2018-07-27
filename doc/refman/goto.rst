Goto and Labels
===============

Code may be marked with labels and jumps to any visible label
can be written. Gotos, however, may not jump though a function.
A goto can cross procedure boundaries, or it can be a local
goto within a function.

.. code-block:: felix

    var x = 10;
  again:>
    println$ x;
    if x == 0 goto finish;
    --x;
    goto again;
  finish:>
    println$ "Done";

If the goto is to a label in the current context, it is called
a local goto. 

.. code-block:: felix

  proc f () {
    println$ "This is f";
    goto finish;
  }

    f ();
  finish:>
    println$ "Done";

If the goto is to a label in a surrounding context, it is called
a non-local goto. Non-local gotos may convert to local gotos as
a result of compiler optimisations such as inlining. Local gotos
can never become non-local.

A procedure containing a non-local goto may be passed as an argument
to another procedure:

.. code-block:: felix

    proc f () {
      println$ "This is f";
      goto finish;
    }
    proc g (h: 1-> 0) {
      h ();
    }
    g();
  finish:>
    println$ "Done";

This can be used to provide error handling or an abnormal exit.
Be sure that the context of the target label is active or the
result may be unpredictable.

Labels are first class values of type `LABEL` and can be stored
in variables:

.. code-block:: felix

    var lab : LABEL = 
      if c then tr else fa endif
    ;
    goto lab;
  tr:>
    println$ "True";
    goto finish;
  fa:>
    println$ "False";
  finish:>
    println$ "Done";


As with non-local gotos, the programmer must ensure the context of the target
is live at the time a goto is done.

Label values encapsulate both the target code address and its context
at the time they're created. Note that contexts are identified by
frame address and frames are mutable. In particular the return address
of a frame can be zeroed out by the system if the frame returns.


The library contains the following low level operation:

.. code-block:: felix

  proc branch-and-link (target:&LABEL, save:&LABEL)
  {
     save <- next;
     goto *target;
     next:>
  }

which can be used to implement coroutines. Branch and link works
by jumping to the label stored in the selected `target`, 
whilst saving the current location in the store pointed at by `save`. 
The target routine can then call for a branch to the saved value, 
providing a store to save its own current location. For example
this allows two routines to regularly exchange control.

.. code-block:: felix

    var l1: LABEL;
    var l2: LABEL = p1;
    println$ "Start";
    branch-and-link (&l2, &l1);
    println$ "p2";
    branch-and-link (&l2, &l1);
    // not reached

  p1:>
    println$ "p1";
    branch-and-link (&l1, &l2); 
    println$ "Finish";


The value stored in a label is converted to a continuation by setting
the contination frames current program counter to the code address
of the label, overwriting the previous program counter. The goto then
make the modified continuation the current continuation of the
current fibre and resumes it.

Local direct gotos are optimised by eliding the continuation, since
by definition the context of the goto and the context of the target
are the same.

The current continuation of an executing procedure can be obtained with
the unit function `current_continuation`, it returns the current procedure
frame which has type `cont`. It is just the C++ `this` pointer of the
procedures activation record:

.. code-block:: felix

  fun current_continuation: unit -> cont = "this";

A continuation can be invoked by throwing it:

.. code-block:: felix

  proc _throw: cont

The current position within the continuation is of type LABEL and is
a function of a continuation value:

.. code-block:: felix

  fun current_position : cont -> LABEL;

The implicit entry point of a continuation or procedure closure 
can be found with the `entry_label` function:

.. code-block:: felix

  fun entry_label : cont -> LABEL;
  fun entry_label[T] (p:T->0):LABEL;



