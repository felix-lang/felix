Try and Catch
=============

The `try/catch` construction is used to provide a *catchable block*
with the ability to catch and process C++ exceptions.

Syntax
------

.. code-block:: felix

  block := "try" stmt+ catches "endtry" =>#
  catch := "catch" sname ":" sexpr  "=>" stmt+
  catches := catch+

Description
-----------

Felix does not generate user catchable C++ exceptions.
When Felix does throw an exception it is a fatal error
and will be caught by the exception handling library,
a diagnostic printed, and the process terminated.

C++ primitives, however, can throw exceptions which
need to be caught.

For example:

.. code-block:: felix

  body bad_def = "struct bad{};";

  body hello_def = """
    void hello() {
      throw bad();
    }
  """ requires bad_def;

  type bad = "bad" requires bad_def;
  proc hello: 1 = "hello();" requires hello_def;

  try
    hello();
  catch badval: bad => 
    println$ "bad";
  endtry
  println$ "Done";

The constraint on the body of the catch is this: Felix most general method
of calling a procedure is to save the current continuation, construct
the procedure object on the heap, and return a pointer to the procedure
object to the system scheduler.

However, `try/catch` is implemented with a C++ `try/catch` which 
means a standard procedure call will lead to a switch case
label inside the `try` body, which is not allowed in C++.
Furthermore, if an except were thrown by the called procedure
it would unwind the machine stack and end up unwinding the
scheduler subroutine call as well, finally being caught by
the exception abort trap wrapping the scheduler.

The problem is, procedures use the Felix spaghetti stack of
linked heap objects, not the machine stack.

Actually some procedure calls uses a compiler generated
micro-scheduler repeatedly calling the target `resume()`
method until it returns NULL, and then continues with
the current procedure. This, in effect, does use the machine
stack, but the micro-scheduler doesn't catch general C++
exceptions. The micro-scheduler is only used if the compiler
is sure the procedure does not do any service calls.

Additionally, some procedures reduce to ordinary C procedures
which are then called directly. These use the machine stack,
and `try/catch` will work on them because they're equivalent
to calling a primitive, which of course is also a C procedure.

The long and short of all this is that the ony *reliable* use
of `try/catch` is around a list of calls to primitives or
inlined Felix procedures with this property, recursively.


