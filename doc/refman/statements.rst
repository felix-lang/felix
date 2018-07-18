Statements
==========

Assignments
+++++++++++

Felix primary method of setting store is the intrinsic `_storeat`:

.. code-block:: felix

  proc storeat[T] (p: &>T, v:T) { _storeat (p,v); }

The library procedure take a pointer or write-only point to T
and a value V of type T, and calls the system intrinsic _storeat.
The parser in turn maps

.. code-block:: felix

  p <- v;

to the procedure `storeat`. For simple variables only you can write:

.. code-block:: felix

  x = v;

which is notionally sugar for

.. code-block:: felix

  &x <- v;

In addition each of the following infix operators calls a two argument
procedure with the same name as the operator:

======== ===========================
operator usual meaning for uints
======== ===========================
+=       increment
-=       decrement
/=       quotient
\*=      product
%=       remainer
\<\<=    mul 2^N
\>\>=    div 2^N
\^=      bitwise exclusive or
\&=      bitwise and
\|=      bitwise or
======== ===========================



Conditionals
++++++++++++

The simplest form of a conditional construction is:

.. code-block:: felix

  if cond do
     stmts
   elif cond do
     stmts
   ...
   else
     stmts
   done

The `elif` and `else` clauses are optional. The final `done`
does not require a trailing semicolon. The construction is
sugar for a collection of labels and gotos, so that it is
ok to put labels in the controlled statements and jump
into the middle of a conditional with a goto.

A simplified form is drives a single statement:

.. code-block:: felix

  if cond perform stuff;

A more advanced statement is:

.. code-block:: felix

  match expr with
  | pattern1 => stmts1
  | pattern2 => stmts2
  ...
  endmatch;

The final endmatch and semicolon is mandatory to distinguish the construction
from a match expression. If none of the pattern match
the program aborts with a match failure exception.



Loops
+++++

While
-----

Here is a while loop:

.. code-block:: felix
  
   while (x>1) do
     println$ x;
     --x;
   done

The loop body executes repeatedly until the condition is
not satisfied. If the condition is initially unsatisfied
the body is not executed.

C style for loop
----------------

Base For Loops
--------------

Felix has some basic
