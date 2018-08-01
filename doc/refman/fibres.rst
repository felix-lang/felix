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

Felix top level mainline code forms a coroutine
which the system spawns automatically. However
you can also create a sub-scheduler with the procedure
call

.. code-block:: felix

   run p;

`run` is a subroutine, it creates a new scheduler object,
spawns p on that scheduler, and runs the scheduler until
there are no active fibres left.

Starvation
----------

A fibre starves if it is suspended on a channel which is 
unable to be written. What this means is that no other active
procedure own the pointer to the channel, in other words,
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

Note if a procedure stack from contains a channel, or data structures
which make the channel reachable, then the channel is considered
accessible, even if the procedure has no control path which will
lead to an I/O attempt on it. For this reason channels should be
forgotten except by those using them.

Contrarily, when a channel is reachable and a fibres is suspended on it,
if the procedure which can reach it never does so, that is called
a `livelock`.

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
called a `closed pipeline`. There are special operators
to simplify pipeline construction:

.. code-block:: felix

  var pipeline = writer |-> squareit |-> reader;
  pipeline ();


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




 












 


