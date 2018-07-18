Control Model
=============

The Felix control model is split into two distinct pieces.

Function Representation
-----------------------

Functional code uses the machine stack for function return addresses.

A function type object is an abstract class with a pure virtual method
called apply which returns a representation of the codomain
and accepts a representation of the domain.

A function is derived from its type and implements
the apply method.

Function closures in Felix are pointers to function type objects,
therefore all functions of the same type are represented by a
pointer to the same C++ class. The actual function is called
by virtual dispatch.

The function class constructor is used to store a pointer
to the thread frame object and the display, which is the
list of the most recent activation records 
of the ancestors of the function at the time
the closure was created. The function can use the display
to access the ancestor local variables.

The objects pointer to by the display members can be 
either function or procedure frames. Here is an example,
the macros in the C++ code have been expanded:

.. code-block:: felix

  noinline fun k(z:int) = {
    fun f(x:int) = {
      var y = x;
      return  y + z;
    }
    return f;
  }


.. code-block:: cpp

  struct thread_frame_t;

  //TYPE 52224: int -> int
  struct _ft52224 {
    typedef int rettype;
    typedef int argtype;
    virtual int apply(int const &)=0;
    virtual _ft52224 *clone()=0;
    virtual ~_ft52224(){};
  };


  //FUNCTION <50810>: k int -> (int -> int)
  //    parent = None
  struct k {
    thread_frame_t *ptf; 

    int z;
    k(thread_frame_t *);
    k* clone();
    _ft52224* apply(int const &);
  };

  //FUNCTION <50812>: k::f int -> int
  //    parent = k<50810>
  struct f: _ft52224 {
    thread_frame_t *ptf; 
    k *ptrk;

    int x;
    int y;
    f  (thread_frame_t *, k*);
    f* clone();
    int apply(int const &);
  };

  //FUNCTION <50812>: k::f: Apply method
  int f::apply(int const &_arg ){
    x = _arg;
    y  = x; //init
    return y + ptrk->z ;
  }

  //FUNCTION <50810>: k: Apply method
  _ft52224* k::apply(int const &_arg ){
    z = _arg;
    return (new(ptf->gcp, f_ptr_map) f(ptf, this));
  }

The symbol `gcp` is a pointer to the garbage collector profile object.
The symbol `f_ptr_map` is a pointer to the static run time
type information for `f` which is associated with the store allocated
for the closure of f created to the collector can trace it.
This is necessary because the closure of `f` contains a pointer
to a closure of `k`, as well as the thread frame object.

The type of `k` is elided because Felix knows the function
not formed into a closure, this is an optimisation.


Abstract Representation of Procedural Continuations
---------------------------------------------------

========================
Field
========================
Service Address
Caller Continuation
Program Counter
Display
Thread Frame
Local Variables
========================

Service Address
^^^^^^^^^^^^^^^

Address of a service request, usually NULL.

Caller Continuation
^^^^^^^^^^^^^^^^^^^

Pointer to the calling procedure's continuation, or NULL if there isn't one.

Program Counter
^^^^^^^^^^^^^^^

A location in the code set when the continuation is suspended
to allow resumption from the suspension point.

Display
^^^^^^^

An array of pointers to continuations consisting
of the activation records of the parent,
grandparent, great grandparent, etc, through to the
outermost procedure at the time this continuation
is created.

Thread Frame
^^^^^^^^^^^^

A pointer to the thread frame, which is a global record shared
by all threads of the current process. It contains at least
a pointer to the system garbage collector, the program arguments,
and pointers to the standard input, output and error streams
and possibly some other technical data. The rest of the frame
contains the global level variables. 

Local Variables
^^^^^^^^^^^^^^^

The local variables of the procedure.

Notes
^^^^^

The return address of a procedure consists of a pointer
to the calling continuation and the program counter
stored in *that* continuation (not in the current one).

Optimisation
------------

Function and procedure objects are generally allocated
on the heap. However if it is safe, Felix can allocate
them on the machines stack.

Furthermore, it may also replace them with actual C++
functions.

Finally, it can also inline functions so they may not exist
at all as discrete objects. Within certain bounds
direct calls and applications are inlined.


Goto
----

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


