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

Circuits
========

A pipeline is a special case of a circuit. 


