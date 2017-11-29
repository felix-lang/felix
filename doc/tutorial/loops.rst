Loops
=====

Loops statements are compound statements
that make control go around
in circles for a while, then exit at the end
of the loop.


while loop
----------

The simplest loop, repeatedly executes its body whilst
its condition is true. If the condition is initially false,
the body is not executed. On exit, the statement following
the loop is executed.

.. code-block:: felix

   var x = 10
   while x > 0 do
     println$ x;
     x = x - 1;
   done
   println$ "Done";

A semicolon is not required after the `done`. Make sure
when writing while loops that the condition eventually
becomes false, unless, of course, you intend an infinite loop.

for loop
--------

For loops feature a control variable which is usually
modified each iterator, until a terminal condition is
met. The simplest for loop uses a slice:

.. code-block:: felix

    for i in 0..<3 do
      println$ i;
    done

Here, we print the variable `i`, which is initially 0,
and takes on the values 1,2 as well before the loop terminates.
The slice used indicates it is exclusive of the last value.
An inclusive slice is illustrated here:

.. code-block:: felix

    for i in 0..3 do
      println$ i;
    done

and the loop iterations include the value 3. The values
of a the slice start and slice end delimiters must
can be arbitrary expressions but be of type `int`.
Slices can be empty if the end is lower than the start,
in this case the loop body is not executed.

The control variable, `i` above, is automatically
defined and goes out of scope at the end of the loop.
It should not be modified during the iteration.




