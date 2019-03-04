Chips
=====

Well now we're going to change notations again! Here we go:

.. code-block:: felix

  // writer fibre
  chip writer 
    connector io
      pin out: %>int
  {
    for i in 1..10 do
      println$ "Sending " + i.str;
      write (io.out,i);
    done
  }

  // reader fibre
  chip reader 
    connector io
      pin inp: %<int
  {
    while true do 
      var x = read inp;
      println$ "Read " + x.str;
    done
  }

  chip doubler 
    connector io
      pin inp: %<int
      pin out: %>int
  {
    while true do
      var x = read io.inp;
      write (io.out, 2 * x);
    done
  }

  run$ writer |-> doubler |-> reader;

The chips are identical to our previous reader, writer and doubler procedures,
we just have some syntactic sugar to make them look like integrated circuit
specifications.

A chip can have more than one connector, the connector name is just
the parameter name. Each connector can have several pins, the pin
names are just the record field names. This syntax is a bit more
verbose but it is easier to read and makes the purpose and intended
use of the procedures clear.

Standard Chips
===============

The library contains a lot of standard chips. 

Write block
-----------

Blocks a writer, deliberately.
When a writer writes to a channel connected to this device,
nothing happens, so the write blocks.

.. code-block:: felix

  chip writeblock[T]
    connector io
      pin inp : %<T
  {
  }

Read Block
----------

Starves a reader, deliberately. When a reader tries to read
from a channel connected to this device, nothing happens,
so the reader starves.

.. code-block:: felix

  chip readblock[T]
    connector io
      pin out: %>T
  {
  }


Universal sink
--------------

Reads input forever, but ignores it.

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

Constant Source
---------------

Writes the fixed value `a` forever.

.. code-block:: felix

  chip source[T] (a:T)
    connector io
      pin out: %>T
  {
    while true do
      write (io.out, a);
    done
  }

One shot source
---------------

Writes the fixed value `a` once then exits.

.. code-block:: felix

  chip value[T] (a:T)
    connector io 
      pin out: %>T
  {
    write (io.out, a);
  }

Source from generator
---------------------

Calls a generator to obtain a value, then writes it,
repeatedly.

.. code-block:: felix

  chip generator[T] (g: 1->T)
    connector io
      pin out: %>T
    {
      repeat perform write (io.out, g());
    }

Source from iterator
--------------------

This chip reads values from an iterator and streams them
to output until the iterator returns None. It is a hand optimised
version of the less efficient `for v in x perform write(io.out,v);`.

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

Source from list
----------------

A list iterator specialised to lists.
It returns when all the values in the list have been written out.

.. code-block:: felix

  chip source_from_list[T] (a:list[T])
    connector io
      pin out: %>T
  {
    for y in a perform write (io.out,y);
  }

Bound Source from list
----------------------

This routine generates an option stream from the list `a`.
Each value is written as `Some v` until the list exhausted,
then an infinite stream of `None[T]` is written.

This is a bound stream because the *logical* content
of the stream is terminated by None; that is,
the option type is used as a carrier type.

.. code-block:: felix

  chip bound_source_from_list[T] (a:list[T])
    connector io
      pin out: %>opt[T]
  {
    for y in a perform write (io.out,Some y);
    while true perform write (io.out,None[T]);
  }


Function adaptor
----------------

One of the most useful chips, the function adaptor reads a stream
of values, applying the function `f` to each value and writing them
out as it goes. It is the dual to functional programming `map` over
lists.


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

Procedure adaptor
-----------------

Converts a procedure to a sink.
Reads values and calls the procedure `p` on each one.

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

Filter
------

Reads values and writes out that that satisfy the predicate `f`.
Dual to functional programming filter over lists.

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


Filter and map
--------------

Applies the predicate `c`, and writes the application of
`f` to values satisfying it out.

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


Sink to List
------------

This chip is a sink that reads values and pushes
them onto an extenal list identified by a pointer.
The list must be initialised before the coroutine is spawned.

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

Sink to unique list
-------------------

Same as `sink_to_list` except the value is only pushed
onto the list if it is not already present.

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

Buffer
------

Perhaps the most important and useful chip, it simply copies
its input to its output.

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


