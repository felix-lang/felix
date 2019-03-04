Circuits
========

Suppose we want some chips to work like this: we have a one shot source A that sends 
a list to B. B then strips the head off the list and sends it to C which prints it.
B also sends the tail back to itself, so the list goes round and round having
the head stripped off until there's nothing left.

As stated this can't work because a fibre cannot write on the output end
of a channel and read it back at the same time. But we can do it if we
stick a buffer in the feedback loop.

So here is our first chip A:

.. code-block:: felix

  device A = value ([1,2,3,4]);

The `device` binder is just a synonym for `var` which looks good when dealing with
circuits.
  
Now our list decoder:

.. code-block:: felix

  chip B 
    connector io
      pin inp: %<list[int]
      pin outhead: %>int
      pin outtail: %>list;int]
   {
     while true do
       var x = read io.inp;
       match x with
       | head ! tail =>
         write (io.outhead, head);
         write (io.outtail, tail);
       | Empty => 
         return;
     done
   }

We need a printer:

.. code-block:: felix

   chip C
     connector io
       pin inp: %<int
   {
     while true do
       var x = read io.inp;
       println$ x.str;
     done
   } 

And finally well have our buffer:

.. code-block:: felix

  device D = buffer[list[int]];

Now we can write a routine to connect all these devices with channels
and spawn them, but there's a better way:

.. code-block:: felix
 
  circuit
    connect A.out, B.inp
    connect B.outhead, C.inp
    connect C.outtail, D.inp
    connect D.out, B.inp 
  endcircuit 

The circuit statement doesn't just solder wires between the pins
of chips: it turns the power on too. Although it has a declarative
form, it is an executable statement which actually runs the circuit.

There are constraints with circuits built using the `connect` clause.
It only works for devices with one connector. If there are two 
connectors the first one has to be bound manually so only one is left.

All the pins of all the devices have to be connected to at least one
other pin. You can use the read and write blockers if there is a pin
that is unused.

All the connected pins must transport the same data type.

And finally, at least one of the set of connected pins must be
and output pin, and one must be an input pin.

There is also a `wire` clause that allows connecting one end of
a manually connected channel.

The big advantage of the circuit statement is that, apart from simplifying
the syntax for building a circuit, the names of the channels used internally
are forgotten automatically, they're not exposed to the client program.
This ensures reachability driven termination will be effective.

Note that there is no assurance the circuit will actually work as expected,
and there's no assurance it will terminate: plenty of useful circuits 
run forever!


