Pipelines
=========

We are now going to change our way of modelling coroutines again,
by using records are arguments instead of tuples.


.. code-block:: felix

  // writer fibre
  proc writer (io: (out: %>int)) ()
  {
    for i in 1..10 do
      println$ "Sending " + i.str;
      write (io.out,i);
    done
  }

  // reader fibre
  proc reader (io: (inp: %<int)) ()
  {
    while true do 
      var x = read inp;
      println$ "Read " + x.str;
    done
  }

  proc doubler (io: (inp: %<int, out: %>int)) ()
  {
    while true do
      var x = read io.inp;
      write (io.out, 2 * x);
    done
  }

  proc start_network () {
    // schannels for communication
    var inp1, out1 = mk_ioschannel_pair[int]();
    var inp2, out2 = mk_ioschannel_pair[int]();
    spawn_fibre$ writer (out=out1);
    spawn_fibre$ doubler (inp=inp1,out=out2);
    spawn_fibre reader (inp=inp2);
  }

  start_network;

Here the coroutines use a single record as a parameter, each field of the
record is a channel. This has the advantage that it's easier to pass the
arguments correctly, since they're named, rather than having to be put
in a specific order.

Using this protocol, we can also rewrite `start_network`:

.. code-block:: felix

  proc start_network () {
     run$ writer |-> doubler |-> reader;
  }

A writer is also called a *source*. 
A reader is also called a *sink*.
A loop which reads, does something with the value, then writes, is called a *transducer*.

If you have a source connected to a series of transducers and ending with a sink,
that is called a *closed pipeline*. The left associative infix pipe operator `|->`
can be used to connect coroutines into a pipeline. The result of the composition
is itself a coroutine. 

Not all coroutine networks are pipelines, however they are very common both as
whole networks, and also as sub-parts of more complex networks.

Pipelines are dual to monads in functional programming .. but they're
much easier to use and a lot more efficient.
 
