Simple Control Flow
===================

Sequential Flow
---------------

Normally, control flows from one statement to the next.

.. code-block:: felix

    first;
    second;
    third;

No operation
------------

Felix provides several statements which do nothing.
A lone semi-colon `;` is a statement which does nothing.

.. code-block:: felix

    first;
    ; // does nothing
    second;



Labels and Gotos
----------------

The default control flow can be modified by labelling
a position in the code, and using a goto or conditional
goto targetting the label.

.. code-block:: felix

      var x = 1;
    next:>
      println$ x;
      if x > 10 goto finished;
      x = x + 1;
      goto next;
    finished:>
      println$ "Done";


An identifier followed by `:>` is used to label
a position in the program.

The unconditional `goto` transfers control
to the statement following the label.

The conditional goto transfers control to the
statement following the label if the condition
is met, that is, if it is true.

Chained Conditionals
--------------------

A chained conditional is syntactic sugar for a sequence
of conditional gotos. It looks like this:

.. code-block:: felix

    if c1 do
      stmt1a;
      stmt1b;
    elif c2 do
      stmt2a;
      stmt2b; 
    else
      stmt3a;
      stmt3b; 
    done


At least one statement in each group is required, 
the no-operation `;` can be used if there is nothing to do.
A semicolon is not required after the `done`.
