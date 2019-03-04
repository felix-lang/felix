Coroutines
==========

Coroutines are the fundamental building block of Felix.
Your mainline program is actually a coroutine!

.. code-block:: felix

  // schannels for communication
  var inp, out = mk_ioschannel_pair[int]();

  // writer fibre
  spawn_fthread {
    for i in 1..10 do
      println$ "Sending " + i.str;
      write (out,i);
    done
  };

  // reader fibre
  spawn_fthread {
    while true do 
      var x = read inp;
      println$ "Read " + x.str;
    done
  };

In this simple example, the `mk_ioschannel_pair` function is used to construct
a *synchronous channel* object, and returns two references to it, `inp` which
is typed to allow reading, and `out` which is typed to allow writing. 

Then we use the `spawn_fthread` to construct fibres from argument
coroutines. A coroutine in Felix is just a procedure. No wait!
Actually .. a procedure is just a coroutine!

A single pre-emptive *thread* of control can be woven from
many strands called *fibres*. In Felix the kind of fibres
we make are called `fthreads`, whereas the pre-emptive kind
are called `pthreads`.

Our writer coroutine is a loop which writes 10 integers down the channel.
Our reader coroutine is an infinite loop, which reads integers until there
are none to read. We use debugging prints to witness the activity.

Fibres do not run concurrently or asynchronously! Fibration is a non-deterministic
method for interleaving control and is entirely synchronous. In particular
`schannels` do not provide any buffering, their primary function is to mediate
control interleaving.

Suppose our writer starts first. When it reaches the write statement it is
suspended. Then our reader starts up and runs until it reaches the read
operation. Then it also suspends.

Now, the scheduler copies the integer from the reader to the writer,
places both on the list of active fibres, and then picks a fibre
to run, which it does by unfreezing or `resuming`  the suspension.





