Statements
==========

Basic Control Flow
++++++++++++++++++

Goto
----

Code may be marked with labels and jumps to any visible label
can be written.

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

A `return from` can be used to exit an outer procedure.
A return is implicit at the end of a procedure
so that if control drops through, the procedure returns.

Traps
-----

The `call_with_trap` operation is a special variant of a call
in which an exception handling trap is established and then
the procedure called on the given argument.

Inside the procedure an error handling procedure is defined
and passed to client code.

The client code can then use `throw_continuation` to throw the
error handler. The error handler is then called in the
context of the `call_with_trap` which should be the context
of its definition.

.. code-block:: felix
  call_with_trap {
    proc ehandler() {
      eprintln("BATCH MODE ERROR HANDLER");
      result = 1;
      goto err;
     }
     result = runit(ehandler);
   err:>
  };
  proc runit (ehandler: 1->0) {
    throw_continuation ehandler;
  }

In this case the error handler does a non-local goto
to exit, and jumps to a label at the end of the 
anonymous procedure which was called with a trap,
then that procedure exits normally.

Continuations can  be thrown inside functions, and are implemented
with a C++ throw which unwinds the machine stack. However procedures
use a spaghetti stack consisting of heap allocated stack frames.
The top level scheduler guards invocations of procedural continuations
with a C++ catch clause, however compiler generated procedure calls
may elide the guard for performance reasons.

The `call_with_trap` operation ensures the system scheduler handles the
call of the procedure, instead of optimised generated code. 

When the scheduler guard catches a continuation, it discards the currently
running continuation of the current fibre, and replaces it with the
continuation which it caught.

Be sure to use both throws and long jumps with care as neither
are intrinsically safe in the following sense: it is possible to
throw or jump to code in a continuation which has already exited.
A non-local goto resets the continuations program counter to the selected
target and executes the exhausted frame until it returns. 
However the return has already been taken. The system may choose
to zero out the return address of a frame when it returns,
in which case a second return will terminate the fibre .. but not
before it reaches the return instruction.



Spawn_fthread
-------------

Spawn_fthread spawns a fibre. It is a library procedure
which wraps a service call. The argument be a unit procedure:

.. code-block:: felix

   proc corout () { println$ "Hello"; }
   spawn_fthread corout;

Whether the current fibre or the spawned one run
next is not determined, however the spawned procedure
runs first in the current implementation.

Suicide
-------

The `suicide` routine terminates a fibre. It takes a
unit argument and does not return control.

Spawn_pthread
-------------

Spawn_pthread spawns a detached pre-emptive thread. It is a library
procedure which wraps a service call. The argument
must be a unit procedure. pthreads cannot be joined.

Exit
----

The `exit` routine terminates the current process. 
It takes an integer argument and returns it to the
operating system.

Abort
-----

The abort routine terminates the current process
with prejudice. It takes no argument. A message is
printed before the process is terminated.


Assignments
+++++++++++

Conditionals
++++++++++++


Loops
+++++



