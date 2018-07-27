Pre-emptive Threads
===================

Felix supports contruction of pre-emptive threads.

Spawning
--------

A library procedure `spawn_pthread` accepts a unit procedure argument
and spawns it is a detached anonymous pre-emptive thread.

.. code-block:: felix

  spawn_pthread { println$ "hello"; };

This means you cannot join threads, and you do not get an identifier
or handle for a thread. All Felix threads have the same status,
including the initial thread. A process does not end until all
threads terminate.

Pchannels
---------

The primary vehicle for synchronisation is the `pchannel`.
Technically a pchannel is a monitor. When a thread writes
to a pchannel it is blocked until another thread reads
its data. Similarly if a thread reads from a channel it
is blocked until another thread writes it data.

.. code-block:: felix

   var inp, out = mk_iopchannel_pair[int]();
   spawn_pthread { 
     println$ "R1"; 
     var i = read inp; 
     println$ "R2-" + i.str; 
   };
   spawn_pthread {
     println$ "W1";
     write (out, 42);
     println$ "W2";
   };
   println$ "Spawning done";

In this program R1 and W1 print first in an indeterminate order.
Then  R2 and W2 print, in an indeterminate order.
The spawning done message can print at any time.
The I/O access to the pchannel therefore acts a barrier.

Any number of threads can attempt to read or write on a pchannel.
If a write is already writing, another writer will block
until the operation is completed by a reader reading,
similarly only one reader can wait for data at a time,
another reader will block until the reader has acquired its data.

Pchannels can be used to join threads.

Mutual Exclusion
----------------

Felix provides two kinds of mutex locks. The primary lock is a
spin loop with a delay. We use a loop rather than a system
mutex because the loop also checks to see if another thread
has requested a garbage collection.

Felix also provides a raw OS mutex. It is not safe unless
correctly used. The critical region protected by a raw lock
*must not perform a Felix heap allocation* because that can
trigger a garbage collection. The problem is the collector
is a world stop collector which must wait until all threads
suspend, and a thread trapped waiting for an OS mutex cannot
check if a garbage collection is requested, resulting in a
deadlock.

Condition Variables
-------------------

GC aware condition variable with builtin mutex.


Atomic Operations
-----------------

Uses C++11 atomics.

Thread Pool
-----------

Pfor
----

Concurrently Procedure
----------------------

The `concurrently_by_iterator` procedure implements a fork/join protocol.
It accepts an iterator which yields unit procedures, calls the
iterator and spawning a Felix pthread for each procedure yielded.
When the iterator is exhausted, it waits for all the spawned pthread
to complete before continuing.

A variant, `concurrently` accepts any data structure with an iterator method:

.. code-block:: felix

  concurrently$
    { println$ "T1"; },
    { println$ "T2"; },
    { println$ "T3"; }
  ;

The concurrently procedure does not use the thread pool.


