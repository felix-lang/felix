Coroutine Termination
=====================

Run procedure
+++++++++++++

In order to better understand coroutines, we will examine the semantics of a fundamental
procedure `run`.  

.. code-block:: felix

  run start_network;

The `run` procedure is an ordinary procedure that returns when it is finished.
It creates a new *coroutine scheduler* object, and then spawns its argument
on that scheduler. It then runs all the fibres on the scheduler until
there are none left, and returns.

Fibre States
++++++++++++

Every fibre is in one of four states. It is either *running*, *active*, *waiting*, or *dead*.

At any time, for any pthread, there is only ever one running fibre.
If there are no running fibres, the pthread terminates. Every pthread
that is spawned automatically runs a fibre scheduler, including the mainline
thread!

A fibre is *active* if it is suspended but ready to run. The scheduler
keeps a list of active fibres. When the currently running fibre suspends,
the scheduler just picks another fibre off the active list and runs it.
If there are no fibres on the active list, the scheduler procedure returns.

A fibre which is waiting will be either *waiting to read* or *waiting to write*.
The waits always occur on a specific schannel. Every schannel is therefore
either *empty*, contains a list of fibres waiting to read, or contains a list
of fibres waiting to write.

If a write operation is performed on a channel which is empty, the writing fibre
is added to the channel wait list. If the channel already contains fibres
waiting to write, the writing fibre is also added to the channel wait list.

But if the channel contains a list of fibres waiting to read, then one of the
fibres is taken off the channel wait list instead, the object being written
is transfered from the writer to the reader, and both the fibres are
added to the schedulers active list. Since the write was running, and is
now merely active, the scheduler picks another fibre to run.

Read operations work exactly the same way, swapping the meaning or read
and write about.

Reachability
++++++++++++

If a fibre is waiting to read, but no other fibre ever writes to the channel
it is waiting on, the fibres is said to be *starved*. If a fibre is waiting
to write, but no other fibre ever reads to the channel it is waiting on,
the fibres is said to be *blocked*.

With pthreads, this lockup is fatal and invariably a design fault.
Not so with fibres! Recall our reader:

.. code-block:: felix

  // reader fibre
  proc reader (inp: %<int) ()
  {
    while true do 
      var x = read inp;
      println$ "Read " + x.str;
    done
  }

This is an infinite loop but our write only wrote 10 integers.
So after reading 10 integers our fibre starves!

Now here is the trick! Only thread reader and writer fibres can
reach the channel that connects them. No one else could write
on that channel, because no one else knows its name. What is more,
the writer has returned so is now dead, so it cannot reach the
channel either, because it doesn't exist!

So since the read is not running or active, even the scheduler
doesn't know about it. Only the channel knows about it,
and only the reader knows about the channel.

So the scheduler now has no running or active procedures
and it returns. The starving reader and the channel are
unreachable and forgotten. So the garbage collector simply
deletes them.

Unlike pthreads, locking up is potentially a *correct* way to terminate.
The motto is that fibres cannot dead-lock because if they do they're
dead and if they're dead the don't exist so they cannot be dead-locked.

However, coroutines can *livelock*. A livelock occurs when there
is a fibre which can do write on a channel which would relieve starvation
of another fibre, but choses not to, or, a fibre which can read from
a channel which would relieve a blockage, but chooses not too.
In other works the channel is *statically reachable* but is *dynamically ignored*.

The key to correct design with coroutines is simple: if a routine is not
going to perform I/O on a channel it must forget it. In other words the channel
must go out of scope.

Our `start_network` procedure obeys this rule. It never reads or writes
from the channel it constructs but after binding the channel to the reader
and write procedures and spawning the bindings as fibres, it returns,
thereby its knowledge of the channel is forgotten, in particular because
it doesn't exist after returning!


