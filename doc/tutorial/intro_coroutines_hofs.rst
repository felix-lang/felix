Hgher Order Coroutines
======================

Our previous example would not be consider good practice because
the coroutines are not reusable components. Programming with coroutines
is all about modularity. Lets see a better way:

.. code-block:: felix

  // writer fibre
  proc writer (out: %>int) ()
  {
    for i in 1..10 do
      println$ "Sending " + i.str;
      write (out,i);
    done
  }

  // reader fibre
  proc reader (inp: %<int) ()
  {
    while true do 
      var x = read inp;
      println$ "Read " + x.str;
    done
  }

  proc start_network () {
    // schannels for communication
    var inp, out = mk_ioschannel_pair[int]();
    var co_writer = writer inp;
    var co_reader = reader  out;
    spawn_fibre co_writer;
    spawn_fibre co_reader;
  }

  start_network;

This program does the same thing as our first example, however we have
modularised the reader and writer by making them named procedures, and,
explicitly passing in the channels they should communicate with.

The type `%>int` is a shorthand for `oschannel[int]`, an output channel
for ints, and `%<int` is a shorthande for `ischannel[int]`, an input
channel for ints. Since `spawn_fibre` requires a procedure that takes
the argument `()` we have to add that as well.

Another refactoring here is that we have modularised the network constructing
code into a procedure as well. It creates the channels needed, binds the
reader and writer procedures to the channels they need, then spawns the bound
procedures to create our fibres. 

Then it returns. This is important! When it returns, it *forgets* the names temporarily
given to the channels. This means the mainline, following the call to `start_network`,
cannot reach the channels. Although the channels are named in the constructor, they're
forgotten afterwards, except by the coroutines that use them. We also forget the
names temporarily given to the bound coroutines.

Forgetting what you do not need to know is an absolute imperative when using coroutines
because reachability is what is used to drive termination. I hope you noticed, that
even though the reader has an infinite loop, the program still terminates!

