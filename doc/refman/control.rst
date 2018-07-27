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
either function or procedure frames. Here is an example.

Felix code:

.. code-block:: felix

  noinline fun k(z:int) = {
    fun f(x:int) = {
      var y = x;
      return  y + z;
    }
    return f;
  }

Function types

.. code-block:: cpp

  //TYPE 52224: int -> int
  struct _ft52224 {
    typedef int rettype;
    typedef int argtype;
    virtual int apply(int const &)=0;
    virtual _ft52224 *clone()=0;
    virtual ~_ft52224(){};
  };

Function class:

.. code-block:: cpp

  struct thread_frame_t;

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

Function apply methods:

.. code-block:: cpp

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

Clone methods:

.. code-block:: cpp

  //FUNCTION <51331>: k: Clone method
    k* k::clone(){
    return new(*PTF gcp,k_ptr_map,true) k(*this);
  }

  //FUNCTION <51333>: k::f: Clone method
    f* f::clone(){
    return new(*PTF gcp,f_ptr_map,true) f(*this);
  }


Constructors:

.. code-block:: cpp

  //FUNCTION <51331>: k: Constructor
  k::k(thread_frame_t *ptf_) ptf(ptf_) {}


  //FUNCTION <51333>: k::f: Constructor
  f::f
    (
      thread_frame_t *ptf_ 
      k *pptrk
    )
    ptf(ptf_), ptrk(pptrk) {}




The symbol `gcp` is a pointer to the garbage collector profile object.
The symbol `f_ptr_map` is a pointer to the static run time
type information for `f` which is associated with the store allocated
for the closure of f created to the collector can trace it.
This is necessary because the closure of `f` contains a pointer
to a closure of `k`, as well as the thread frame object.

The type of `k` is elided because Felix knows the function
not formed into a closure, this is an optimisation.

The clone method (not show) invokes the copy constructor, it is used
when calling the function to ensure the initial state is invariant.
This may be necessary if the function is called twice through the closure, 
particularly if it is recursive.

Non-Yielding Generators
-----------------------

A non-yielding generator has the same representation
as a function, except that the clone method returns `this`
instead of a pointer to a heap allocated copy of the 
class object.

Whilst function values stored in variables are cloned
to ensure they have an invariant initial state,
generators aren't, to ensure internal state is preserved
between calls.

Yielding Generators
-------------------

A yielding generator is a generator with a `yield` statement.
Yield returns a value and saves the current program counter.

The `apply` function body is called then the function jumps
to the saved program counter. Note that the parameter is set
to the argument of each invocation.

.. code-block:: felix

  gen f (var x:int) = {
    var i = 10;
    while i > 0 do
      yield x;
      --x; --i;
    done
    return 0;
  }

  var k = f;

  var v = k 4;
  while v > 0 do
    println$ v;
    v = k 2;
  done

This program prints 4 once then 1, nine times.

The usual way to write generators is to use a higher
order function:

.. code-block:: felix

  gen f (var x:int) () = {
    while x > 0 do
      yield x;
      --x;
    done
    return 0;
  }

  var k = f 4;

  var v = #k;
  while v > 0 do
    println$ v;
    v = #k;
  done

The header looks like this:

.. code-block:: cpp

  //FUNCTION <51331>: f int -> (unit -> int)
  //    parent = None
  struct f {
    thread_frame_t *ptf; 

    int x;
    f(thread_frame_t *);
    f* clone();
    _ft52601* apply(int const &);
  };

  //FUNCTION <51333>: f::f'2 unit -> int
  //    parent = f<51331>
  struct _fI51333_f__apos_2: _ft52601 {
    thread_frame_t *ptf; 
    int pc;
    f *ptrf;

    _fI51333_f__apos_2  (FLX_FPAR_DECL f*);
    _fI51333_f__apos_2* clone();
    int apply();
  };


The apply method looks like this:

.. code-block:: cpp

  //FUNCTION <51331>: f: Apply method
  _ft52601* f::apply(int const &_arg ){
    x = _arg;
    return 
      new(*ptf->gcp,_fI51333_f__apos_2_ptr)  
        _fI51333_f__apos_2 (ptf, this)
    ;
  }

  //FUNCTION <51333>: f::f'2: Apply method
  int _fI51333_f__apos_2::apply(){
    switch (pc) { 
      case 0:
      continue__ll_x_102_L51335:;
        if(!(0 < ptrf->x)) 
          goto break__ll_x_102_L51336;
        pc = 52614
        return ptrf->x;//yield
      case 52614:
        {
        int* _tmp52615 = (int*)&ptrf->x;
        --*((_tmp52615));
        }
        goto continue__ll_x_102_L51335;
      break__ll_x_102_L51336:;
        return 0;
    }
  }

With gcc compiler, a computed goto is used instead of a switch.


Abstract Representation of Procedural Continuations
---------------------------------------------------

===== ==================
Abbr. Field
===== ==================
SVA   Service Address
RET   Callers Continuation
PC    Program Counter
DSP   Display
TF    Thread Frame
LV    Local Variables
====  ===================

Service Address
^^^^^^^^^^^^^^^

Address of a service request, usually NULL.

Callers Continuation
^^^^^^^^^^^^^^^^^^^^

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


