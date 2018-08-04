========== ===============================
key        file                            
========== ===============================
chips.flx  share/lib/std/control/chips.flx 
========== ===============================


======
Chips.
======


Standard components
===================


Write block.
------------

Blocks reader.

.. code-block:: felix

  //[chips.flx]
  open class BaseChips
  {
  
  chip writeblock[T]
    connector io
      pin inp : %<T
  {
  }
  

Read block.
-----------

Blocks writer.

.. code-block:: felix

  //[chips.flx]
  chip readblock[T]
    connector io
      pin inp: %>T
  {
  }
  
  

Universal sink
--------------

Reads input forever.

.. code-block:: felix

  //[chips.flx]
  chip sink[T]
    connector io
      pin inp : %<T
  {
    while true do
      var x = read (io.inp);
      C_hack::ignore (x);
    done
  }
  

Constant Source.
----------------

Write fixed value forever.

.. code-block:: felix

  //[chips.flx]
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


.. code-block:: felix

  //[chips.flx]
  chip value[T] (a:T)
    connector io 
      pin out: %>T
  {
    write (io.out, a);
  }
  

Source from generator
---------------------


.. code-block:: felix

  //[chips.flx]
  chip generator[T] (g: 1->T)
    connector io
      pin out: %>T
    {
      repeat perform write (io.out, g());
    }
  

Source from iterator
--------------------


.. code-block:: felix

  //[chips.flx]
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


.. code-block:: felix

  //[chips.flx]
  chip source_from_list[T] (a:list[T])
    connector io
      pin out: %>T
  {
    for y in a perform write (io.out,y);
  }
  
  chip bound_source_from_list[T] (a:list[T])
    connector io
      pin out: %>opt[T]
  {
    for y in a perform write (io.out,Some y);
    while true perform write (io.out,None[T]);
  }
  
  

Function adaptor.
-----------------

Converts function to chip.

.. code-block:: felix

  //[chips.flx]
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
  

Procedure adaptor.
------------------

Converts a procedure to a sink.

.. code-block:: felix

  //[chips.flx]
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

Convert a predicate and function to a transducer.


.. code-block:: felix

  //[chips.flx]
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
  

Sink to list
------------


.. code-block:: felix

  //[chips.flx]
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


.. code-block:: felix

  //[chips.flx]
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
  
  
  

Buffer.
-------

One step buffer. Same as a function adaptor passed identity.

.. code-block:: felix

  //[chips.flx]
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
  
  

Connector symbol
----------------

The syntax |-> is parsed to pipe (a,b).
We add overloads for chips with pins
named io.inp, io.out.


.. code-block:: felix

  //[chips.flx]
  // two transducers
  chip pipe[T,U,V] (a:iochip_t[T,U],b:iochip_t[U,V])
   connector io
     pin inp: %<T
     pin out: %>V
  {
    circuit
      connect a.out,b.inp
      wire io.inp to a.inp
      wire io.out to b.out
    endcircuit
  }
  
  // source to transducer
  chip pipe[T,U] (a:ochip_t[T],b:iochip_t[T,U])
   connector io
     pin out: %>U
  {
    circuit
      connect a.out,b.inp
      wire io.out to b.out
    endcircuit
  }
  
  // transducer to sink
  chip pipe[T,U] (a:iochip_t[T,U],b:ichip_t[U])
   connector io
     pin inp: %<T
  {
    circuit
      connect a.out,b.inp
      wire io.inp to a.inp
    endcircuit
  }
  
  // source to sink
  proc pipe[T] (a:ochip_t[T],b:ichip_t[T])  ()
  {
    circuit
      connect a.out,b.inp
    endcircuit
  }
  
  

Debug Buffer.
-------------



.. code-block:: felix

  //[chips.flx]
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
  

One Shot.
---------

A one shot buffer.  

.. code-block:: felix

  //[chips.flx]
  chip oneshot [T]
    connector io
      pin inp: %<T
      pin out: %>T
  {
    var x = read io.inp;
    write (io.out, x);
  }
  

Store
-----

Stores read values in a variable.

.. code-block:: felix

  //[chips.flx]
  chip store[T] (p:&T)
    connector io
      pin inp: %<T
  {
    while true do
      var x = read io.inp;
      p <- x;
    done
  }
  

Fetch
-----

Writes current value of a variable.

.. code-block:: felix

  //[chips.flx]
  chip fetch[T] (p:&T)
    connector io
      pin out: %>T
  {
    while true do
      write (io.out, *p);
    done
  }
  

Printer
-------

Writes input to console.


.. code-block:: felix

  //[chips.flx]
  chip debug_sink [T with Str[T]] (s:string)
    connector io
      pin inp: %<T
  {
    while true do
      var x = read io.inp;
      println$ "Debug sink ["+s+"] "+x.str;
    done
  }
  
  

Asynchronous Latch.
-------------------

Satisfied all reads with the last
value written. Blocks readers until at least
one value is written.

.. code-block:: felix

  //[chips.flx]
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
  

Serialise.
----------

Read values in sequence from a sequence of channels,
write each one out on a single channel. Repeat.
The input channels are fixed by supplying them as
an argument.


.. code-block:: felix

  //[chips.flx]
  chip serialise_chan_list[T] (a: list[%<T])
   connector io
     pin out: %>T
  {
    while true do
      var current = a;
  next:>
      match current with
      | Cons (h,t) =>
        var x = read h;
        write (io.out, x);
        current = t;
        goto next;
      | Empty => ;
      endmatch;
    done
  }
  
  typedef iopair_t[D,C] = (inp: %<D, out: %>C);
  
  // transducer
  typedef iochip_t[D,C] = iopair_t[D,C] -> 1 -> 0;
  
  // sink
  typedef ichip_t[T] = (inp: %<T) -> 1 -> 0;
  
  // source
  typedef ochip_t[T] = (out: %>T) -> 1 -> 0;
  
  chip pipeline_list[T] (a: list[iochip_t[T,T]])
    connector io
      pin inp: %<T
      pin out: %>T
  {
    proc aux (lst:list[iochip_t[T,T]]) (inp: %<T) {
      match lst with
      | h1 ! h2 ! tail =>
        var inchan,outchan = mk_ioschannel_pair[T]();
        spawn_fthread$  h1 (inp=inp, out=outchan);
        aux (h2!tail) inchan;
      | h1 ! _ =>
        spawn_fthread$  h1 (inp=inp, out=io.out);
      | Empty => 
        spawn_fthread$ buffer (inp=io.inp, out=io.out);
      endmatch;
    }
    aux a io.inp;
  }
  
  // This loops, but only by repeatedly spawning
  // the alternative set. The alternatives are restricted
  // to a single read on each iteration. The chips are
  // respawned because they might be locked up, in which
  // case the whole thing locks up.
  //
  // NOTE: if one of the alternatives starts, and does not
  // read the input, everything locks up. This is because
  // the implementation ACTUALLY progresses serially.
  //
  // this COULD be fixed by adding a buffer to the front of
  // each. Actually better, add a one shot source based
  // on the input.
  chip tryall_list[D,C with Str[D]] (a: list[iochip_t[D,C]]) 
    connector io
      pin inp: %<D
      pin out: %>C
  {
    while true do
      var x = read io.inp;
      //println$ "Tryall read " + a.len.str + " alternatives: " + x.str;
      //var counter = 1;
      for h in a do
        //println$ "Trying alternative #" + counter.str + "/"+a.len.str;
        var lin,lout = mk_ioschannel_pair[D]();
        spawn_fthread (h (inp=lin, out=io.out));
        //println$ "Tryall_list write " + lout.address.str;
        write (lout,x);
      done
    done
  }
  

Deref
-----

This version spawns a clone of p for each input. 
In particular it delays the spawn until there is an input.

.. code-block:: felix

  //[chips.flx]
  chip deref_each_read[D,C] (p:&iochip_t[D,C]) 
    connector io
      pin inp: %<D
      pin out: %>C
  {
    while true do
      var x = read io.inp;
      var rinp,rout = mk_ioschannel_pair[D]();
      spawn_fthread ((*p) (inp=rinp, out=io.out));
      // println$ "Deref_each_read: write " + io.out.address.str;
      write (rout,x);
    done
  }
  
  chip deref_first_read[D,C] (p:&iochip_t[D,C]) 
    connector io
      pin inp: %<D
      pin out: %>C
  {
    var x = read io.inp;
    var rinp,rout = mk_ioschannel_pair[D]();
    spawn_fthread ((*p) (inp=rinp, out=io.out));
    write (rout,x);
    while true do
      x = read io.inp;
      write (rout,x);
    done
  }

Epsilon
-------

Identity chip.


.. code-block:: felix

  //[chips.flx]
  chip epsilon[T]
    connector io
     pin inp: %<T
     pin out: %>T
  {
    while true do
      var x = read io.inp;
      //println$ "Epsilon: write " + io.out.address.str;
      write (io.out, x);
    done
  }

Optional matcher.
-----------------

Matches given matcher if possible and epsilon.
Note the epsilon match is ALWAYS output!


.. code-block:: felix

  //[chips.flx]
  chip optional[T] (p:iochip_t[T,T])
    connector io
      pin inp: %<T
      pin out: %>T
  {
    device both = tryall_list ([
      p,
      epsilon[T]
    ]);
    circuit
      wire io.inp to both.inp
      wire io.out to both.out
    endcircuit
  }
  

One or more matcher
-------------------


.. code-block:: felix

  //[chips.flx]
  
  chip oneormore_matcher[T] (A:iochip_t[T,T]) 
  connector chans 
    pin inp: %<T
    pin out: %>T
  {
   device As = oneormore_matcher A;
   device As2 = pipeline_list (A,As).list; 
   device Ass = tryall_list (A, As2).list;
   circuit
     wire chans.inp to Ass.inp
     wire chans.out to Ass.out
   endcircuit
  }
  

Zero or more matcher
--------------------


.. code-block:: felix

  //[chips.flx]
  
  chip zeroormore_matcher[T] (A:iochip_t[T,T]) 
  connector chans 
    pin inp: %<T
    pin out: %>T
  {
   device As = oneormore_matcher A;
   device Ass = tryall_list (epsilon[T], As).list;
   circuit
     wire chans.inp to Ass.inp
     wire chans.out to Ass.out
   endcircuit
  }


.. code-block:: felix

  //[chips.flx]
  } // end class BaseChips
  
  
