Fibres
=======

General Description
-------------------

The Felix fibration system is implemented by service calls into
the run time library.

These calls are basically:

* mk_ioschannel_pair[T]
* read channel
* write (channel, value)
* spawn_fthread
* suicide
* run

Channel construction
--------------------

A channel is a single object however typically we might say:

.. code-block:: felix

  var inp, out = mk_ioschannel_pair[int]();

to create a channel object on the heap, and then cast a pointer
to to the object to `ischannel[int]` and a copy to `oschannel[int]`.
These types can be abbreviated `%<int` and `%>int` respectively.

Spawning a Fibre
----------------

To create a fibre, we just call:

.. code-block:: felix

  spawn_fthread p;

where p is any unit procedure to create a fibre of control which
starts at the entry point of `p`.

Writing on a channel
--------------------

We can write data onto a channel with the procedure call:

.. code-block:: felix

  write (out, value);

Reading from a channel
----------------------

We can read data with the psuedo function

.. code-block:: felix

   read inp

Termination
-----------

A fibre terminates when its initial coroutine returns, 
it calls `suicide()`, starves or blocks.


Starvation
----------

A fibre starves if it is suspended on a channel which is 
unable to be written. What this means is that no other active
procedure owns the pointer to the channel, in other words,
the channel is unreachable. In this case, the fibre evaporates
automatically because only active fibres are known to the scheduler,
and fibres are otherwise anonymous. Thus the continuation and thus
all the stack frames of the fibre are unreachable and can be reaped
by the garbage collector.

Thus, fibres cannot deadlock, because if they do they no longer exist.
Starvation is equivalent to suicide.

Blockage
--------

A fibre blocks if it is suspended on channel which is unable to be read.
A with starvation, blocking is equivalent to suicide. 

Note if a procedure stack contains a channel, or data structures
which make the channel reachable, then the channel is considered
accessible, even if the procedure has no control path which will
lead to an I/O attempt on it. For this reason channels should be
forgotten except by those using them.

Contrarily, when a channel is reachable and a fibres is suspended on it,
if the procedure which can reach it never does so, that is called
a `livelock`.

Constructing a new Scheduler
----------------------------

Felix top level mainline code forms a coroutine
which the system spawns automatically. However
you can also create a sub-scheduler with the procedure
call

.. code-block:: felix

   run p;

`run` is a subroutine, it creates a new scheduler object,
spawns `p` on that scheduler, and runs the scheduler until
there are no active fibres left on that scheduler.

Note that if `p` itself spawns new fibres they will become
active on the same scheduler as `p`, however, *fibres can
migrate between schedulers*.


Example
-------

Here is a simple example.

.. code-block:: felix

  proc example () {
    var inp,out = mk_ioschannel_pair[int]();
    spawn_fthread { 
       for i in 0..9 perform write (out,i);
    };
    spawn_fthread {
      repeat perform println$ read inp;
    };
  }
  example();
  println$ "Done";

In the example, we create a channel with a read and write
endpoint, and then spawn two fibres. The first one writes
10 numbers and suicides by returning. The seccond one
reads 10 numbers and prints them, then suicides by starvation.

It is important to note that the abstract logic does not specify
when the `Done` is printed. After a spawn, both the spawner
and spawnee are active. After a read and write match up,
both the reader and writer are active. The implementation is free
to choose which of all the active fibres to run next.
However Felix runs the spawnee before the spawner, and it runs
the reader before the writer, so the `Done` will actually print last.

The other very important thing to note is that the `example` procedure
knows the channels being used, however the channel endpoints are stored
in its stack frame, which will become unreachable when `example` returns.
Thus, only the reader and writer will have access to the channel at
that time, and once the writer has terminated that leaves the reader
blocked: it is trying to read from a channel which no active fibre can
write on. This, the reader becomes unreachable, and so when the mainline
terminates the program is finished.

Binding Channels With HOFs.
---------------------------

A better way to write the code above is to use Higher Order
Functions (HOFs).

.. code-block:: felix

  fun make() = {
    typedef r_t = (inp: %<int);
    typedef w_t = (out: %>int);

    proc writer (x: w_t) () {
       for i in 0..9 perform write (x.out,i);
    };
    proc reader (y: r_t) () {
      repeat perform println$ read y.inp;
    };

    var i,o = mk_ioschannel_pair[int]();
    return reader (inp=i), writer (out=o);
  }
  proc example () {
    var r,w = make();
    spawn_fthread r;
    spawn_fthread w;
  }
  example();
  println$ "Done";
 
Here the reader and writer are functions which take a record argument whose
fields are the required channels and return a unit procedure.

Syntactic Supoport
------------------

The protocol above is supported by special syntax:


.. code-block:: felix

  chip writer 
    connector x
      pin out: %>int
   {
     for i in 0..9 perform write (x.out,i);
   }

   chip reader
     connector y
       pin inp: %<int
   {
      repeat perform println$ read y.inp;
   }

   circuit
     connect writer.out, reader.inp
   endcircuit

   println$ "Done";

The `chip` constructions above are exactly the same as the
procedures in the previous example. The connectors are
the record parameters, the pins are the fields of the record.

The `circuit` statement constructs the channels required to
connect the pins automatically, binds them to the 
parameters, and then spawns the resulting unit procedures
as fibres.

Sources, Sinks, and Transducers
-------------------------------

What is important to note here is that connectors can
have any number of pins. Coroutines are not restricted to 
using one communication channel.

The writer above, with a single output pin, is called a `source`.
The reader above, with a single input pin, is called a `sink`.
And the following shows a `transducer`:

.. code-block:: felix

  chip squareit 
    connector x
      pin inp: %<int
      pin out: %>int
   {
     repeat do
       var i = read x.inp;
       write (x.out, i*i);
     done
   }

   circuit
     connect writer.out, squareit.inp
     connect squareit.out, reader.inp
   endcircuit

Pipelines
---------

When you run a set of coroutines starting with
a source, followed by a sequence of transducers,
and terminated by a sink, the construction is
called a `closed pipeline` and is precisely a unit procedure. 

There are special operators
to simplify pipeline construction:

.. code-block:: felix

  var pipeline = writer |-> squareit |-> reader;
  pipeline ();


Pipelines can also be open, if there is no source
at the beginning and no sink at the end, or half open,
where there is a source at the start but no sink at the end,
or no source at the start but a sink at the end.

In fact the pipeline operator is associative:

========== ========== ======
LHS        RHS        Result
========== ========== ======
Source     Sink       Closed Pipline
Source     Transducer Source
Transducer Transducer Transducer
Transducer Sink       Sink
========== ========== ======

in particular for any legitimate combination:

.. code-block:: felix

   a |-> b |-> c
   (a |-> b) |-> c
   a |-> (b |-> c)

are equivalent.


Library Chips
-------------

We can simplify our code again by using standard library chips.
Here is the whole program again:

.. code-block:: felix

  proc readit (y:int) { println$ y; }

  gen  writeit () : opt[int] = {
    for i in 0..9 perform yield Some i;
    return None[int];
  }
  fun squareit (x:int) => x * x;

  var pipeline = iterate writeit |-> function squareit |-> procedure readit;
  pipeline ();
  println$ "Done";

The `iterate` chip is an adaptor that accepts an iterator and produces
a source.

The `function` chip is an adaptor that accepts a function and
proceduces a transducer.

The `procedure` chip is an adaptor that accepts a procedure
with one argument and produces a sink.

Here's another example:

.. code-block:: felix

  run (
    iterate (1,2,3).iterator |->
    function (fun (x:int) =>  x * x) |->
    procedure (proc (x:int) { println$ x; })
  );

which prints the squares of the values of an array 1,2,3
in a single line by using anonymous functions and the standard
iterator method for arrays.

More Library Chips
------------------

writeblock
^^^^^^^^^^

Starves connected reader.

.. code-block:: felix

  chip writeblock[T]
    connector io
      pin inp : %<T
  {
  }

readblock
^^^^^^^^^

Blocks connected writer.

.. code-block:: felix

  chip readblock[T]
    connector io
      pin inp: %>T
  {
  }


sink
^^^^

Universal sink.  Reads input forever.

.. code-block:: felix

  chip sink[T]
    connector io
      pin inp : %<T
  {
    while true do
      var x = read (io.inp);
      C_hack::ignore (x);
    done
  }

source
^^^^^^

Writes fixed value forever.

.. code-block:: felix

  chip source[T] (a:T)
    connector io
      pin out: %>T
  {
    while true do
      write (io.out, a);
    done
  }

value
^^^^^

One shot source

.. code-block:: felix

  chip value[T] (a:T)
    connector io 
      pin out: %>T
  {
    write (io.out, a);
  }

generator
^^^^^^^^^

Writes values acquired from a generator.

.. code-block:: felix

  chip generator[T] (g: 1->T)
    connector io
      pin out: %>T
    {
      repeat perform write (io.out, g());
    }

iterate
^^^^^^^

Writes values acquired from an iterator,
terminates when and if iterator becomes exhausted.

.. code-block:: felix

  chip iterate[T] (g: 1->opt[T])
    connector io
      pin out: %>T
    {
      again:>
        var x = g();
        match x with
        | Some v => 
          write (io.out, v);
          goto again;
        | None => ;
        endmatch; 
    }

source_from_list
^^^^^^^^^^^^^^^^

A specialised source which writes the values of a list.
Terminates at the end of the list.

.. code-block:: felix

  chip source_from_list[T] (a:list[T])
    connector io
      pin out: %>T
  {
    for y in a perform write (io.out,y);
  }

bound_source_from_list
^^^^^^^^^^^^^^^^^^^^^^

Writes Some x, for each x in the list, then
writes an infinite tail of None.

.. code-block:: felix

  chip bound_source_from_list[T] (a:list[T])
    connector io
      pin out: %>opt[T]
  {
    for y in a perform write (io.out,Some y);
    while true perform write (io.out,None[T]);
  }


function
^^^^^^^^

Function adaptor. Converts a function to transducer.
Repeatedly reads input, writes result of applying function to it.

.. code-block:: felix

  chip function[D,C] (f:D->C)
    connector io
      pin inp: %<D
      pin out: %>C
  {
    while true do
      var x = read io.inp;
      var y = f x; 
      write (io.out, y);
    done
  }

procedure
^^^^^^^^^

Procedure adaptor. Converts a procedure taking one
argument to a sink.

.. code-block:: felix

  chip procedure[D] (p:D->0)
    connector io
      pin inp: %<D
  {
    while true do 
      var x = read io.inp;
      p x;
    done
  }

filter
^^^^^^

Convert a predicate and function to a transducer.
Reads value from input, applies function to it,
and writes result if it satisfies the predicate.
Note the predicate applies to the output of the function,
not the input to it.

.. code-block:: felix

  chip filter[D,C] (c:D->bool) (f:D->C)
    connector io
      pin inp: %<D
      pin out: %>C
  {
    while true do
      var x = read io.inp;
      if c x do
         write (io.out, f x);
      done
    done
  }

filter
^^^^^^

A variant of the two argument filter which
reads a value, applies the function to it,
and checks the resulting option type.
If Some v is returned, writes v, if None is
returned does not write anything.

.. code-block:: felix

  chip filter[D,C] (f:D->opt[C])
    connector io
      pin inp: %<D
      pin out: %>C
  {
    while true do
      var x = read io.inp;
      match f x with
      | Some y => write (io.out, y);
      | None => ;
      endmatch;
    done
  }

sink_to_list
^^^^^^^^^^^^

This chip accepts a pointer to a variable
containing a list. Each value read is prepended to 
the list.

.. code-block:: felix

  chip sink_to_list[T] (p: &list[T])
    connector io
      pin inp : %<T
  {
    while true do
      var x = read (io.inp);
      p <- Cons (x,*p);
    done
  }

sink_to_unique_list
^^^^^^^^^^^^^^^^^^^

A variant of `sink_to_list` for which
the value is prepended to the list if, and only if,
it is not already in the list.

.. code-block:: felix

  chip sink_to_unique_list[T with Eq[T]] (p: &list[T])
    connector io
      pin inp : %<T
  {
    while true do
      var x = read (io.inp);
      if not (x in *p) perform 
        p <- Cons (x,*p)
      ;
    done
  }

buffer
^^^^^^

A single value buffer, equivalent to a function adaptor
passed the identity function.

.. code-block:: felix

  chip buffer [T]
    connector io
      pin inp: %<T
      pin out: %>T
  {
    while true do
      var x = read io.inp;
      write (io.out, x);
    done
  }

dup
^^^

Copies input to two outputs.

.. code-block:: felix

  chip dup [T]
    connector io
      pin inp: %<T
      pin out1: %>T
      pin out2: %>T
  {
    while true do
      var x = read io.inp;
      write (io.out1, x);
      write (io.out2, x);
    done
  }

debug_buffer
^^^^^^^^^^^

A variant of a buffer which also prints diagnostics before
reading, after reading and before writing, and after writing.

.. code-block:: felix

  chip debug_buffer [T with Str[T]] (tag:string)
    connector io
      pin inp: %<T
      pin out: %>T
  {
    while true do
      println$ "Debug buffer [" + tag + "] READ";
      var x = read io.inp;
      println$ "Debug buffer [" + tag + "] read " + x.str;
      write (io.out, x);
      println$ "Debug buffer [" + tag + "] written " + x.str;
    done
  }

oneshot
^^^^^^^

A one shot buffer.  Reads one value and writes it,
then terminates.

.. code-block:: felix

  chip oneshot [T]
    connector io
      pin inp: %<T
      pin out: %>T
  {
    var x = read io.inp;
    write (io.out, x);
  }

store
^^^^^

Repeatedly stores read values into a variable.

.. code-block:: felix

  chip store[T] (p:&T)
    connector io
      pin inp: %<T
  {
    while true do
      var x = read io.inp;
      p <- x;
    done
  }

fetch
^^^^^

Repeatedly writes the ccurrent value of a variable.

.. code-block:: felix

  chip fetch[T] (p:&T)
    connector io
      pin out: %>T
  {
    while true do
      write (io.out, *p);
    done
  }

debug_sink
^^^^^^^^^^

Writes input to standard output.

.. code-block:: felix

  chip debug_sink [T with Str[T]] (s:string)
    connector io
      pin inp: %<T
  {
    while true do
      var x = read io.inp;
      println$ "Debug sink ["+s+"] "+x.str;
    done
  }


latch
^^^^^

Satisfies all reads on its output channel with the last
value read on the input channel. Blocks readers until at least
one value is read from its input channel.

.. code-block:: felix

  chip latch[T]
    connector io
      pin inp: %<T
      pin out: %>T
  {
     var x = read io.inp;
     device w = fetch &x;
     device r = store &x;
     circuit
       wire io.inp to r.inp
       wire io.out to w.out
     endcircuit
  } 


Duplex Channels
---------------

A duplex channel can be used to first send data of type D 
from one coroutine to another, and then have the second coroutine
send data of type C back along the same channel.

This protocol emulates a standard function call where D
is the domain of the function and C the codomain.
It can be done with two monotyped half-duplex channels as well:
using a duplex channel saves one heap allocation and enforces
the subroutine call protocol.


Session Typed Channels
----------------------

Underneath, channels are untyped, and I/O operations
transfer a single machine address. Therefore, with casts,
you can read and write a pointer to any data type safely
provided the read and write agree on the type.

Type systems have been developed, called `session types`
which can be used to statically enforce agreement
on the type of data being communicated, where the type
varies over time, however Felix currently does not
support any session types other than duplex channels.



 












 


