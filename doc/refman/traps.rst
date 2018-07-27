Traps
=====

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




