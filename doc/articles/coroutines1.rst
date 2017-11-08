================
Coroutine Basics
================


Coroutines are not a new concept, however they have been ignored for
far too long. They solve many programming problems in a natural way and
any decent language today should provide a mix of coroutines and procedural
and functional subroutines, as well as explicit continuation passing.

Alas, since no such system exists to my knowledge I have had to create
one to experiment with: Felix will be used in this document simply
because there isn't anything else!

A `coroutine` is basically a procedure which can be `spawned` to begin
a `fibre} of control which can be {\em suspended} and {\em resumed` under program
control at specific points. Coroutines communicate with each other
using `synchronous channels` to read and write data from and to other
coroutines. Read and write operations are synchronisation points,
which are points where a fibre may be suspended or resumed.

Although fibres look like threads, there is a vital distinction: multiple
fibres make up a single thread, and within that only one fibre is ever
executing. Fibration is a technique used to structure sequential programs,
there is no concurrency involved.

In the abstract theoretical sense, the fundamental property possessed by
coroutines can be stated like this: within any thread, there exists some
total ordering of all events. The ordering may not be determinate, but of any two
events which occur, one definitely occurs before the other.

In addition, events associated with one fibre which occur between two synchronisation
points, are never interleaved by events from another fibre of the same thread.
All interleaving must occur interior to the synchronisation point, that is,
after it commences and before it completes. In other words, given a sequence
of events from one fibre prior to a synchronisation point, and a sequence of
event from another after a synchronisation point, all the events of each
sequence occur before or after all the events of the other.

Premptive threads, on the other hand, allow arbitrary interleaving of
each threads sequence of events, up to and after any shared synchronisation.
Mutual exclusion locks provide serialisation, which is the default behaviour
of coroutines.

Therefore, fibre based programming can proceed where general code
may assume exclusive access to memory and other resources over all
local time periods not bisected by a volutary synchronisation event;
threads, on the other hand, can only assume exclusive access in the
scope of a held mutex.

The most significant picture of the advantages of coroutines is thus: in a subroutine
based language there is a single machine stack. By machine stack, I mean that
there is an important `implicit` coupling of control flow and local variables.
In the abstract, a subroutine call passes a continuation of the caller to 
the callee which is saved along with local variables the callee allocates,
so that the local variables can be discarded when the final result is
calculated, and then passed to the continuation. This technique may be
called `structured programming`. With coroutines, the picture is simple:
each fibre of control has its own stack. Communication via channels exchanges data
and control between stacks.

Coroutines therefore leverage control and data coupling in a much more
powerful and flexible manner than mere functions, reducing the need for
state to be preserved on the heap, thereby making it easier to construct
and reason about programs.

For complex applications, the heap is always required.


A Simple Example
================

The best way to understand coroutines and fibration is to have a look
at a simple example. 

The Producer
------------

First, we make a coroutine procedure which writes the integers
from 0 up to but excluding 10 down a channel.

.. code-block:: felix

    proc producer (out: %>int) () {
      for i in 0..<10 
        perform write (out, i);
    }

Notice that as well as passing the output channel argument `out`
there is an extra unit argument `()`. This procedure terminates
after it has written 10 integers. The type of variable `out` is
denoted `%>int` which is actually short hand for `oschannel[int]`
which is an output channel on which values of type \verb%int% may
be written.


The Transducer
--------------

Next, we make a device which repeatedly reads an integer, squares it,
and writes the result. It is an infinite loop, this coroutine never
terminates of its own volition. This is typical of coroutines.

.. code-block:: felix
    proc transducer (inp: %<int, out: %>int) () {
      while true do
        var x = read inp;
        var y = x * x;
        write (out, y);
      done
    }

Here, the type of variable `inp` is
denoted `int` which is actually short hand for \verb%ischannel[int]%,
which is an input channel from which values of type \verb%int% may
be read.

The Consumer
------------

Now we need a coroutine to print the results:

.. code-block:: felix

    proc consumer (inp: %<int) () {
      while true do
        var y = read inp;
        println y;
      done
    }

Each of these components is a coroutine because it is a procedure
which may perform, directly or indirectly, I/O on one or more synchronous
channels.

