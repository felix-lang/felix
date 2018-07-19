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

While loops
-----------

Syntax:

.. code-block:: felix

  loop_stmt := optlabel "while" sexpr block 
  loop_stmt := optlabel "repeat" block 
  loop_stmt := optlabel "until" sexpr block 

Here is a while loop:

.. code-block:: felix
  
   while x>1 do
     println$ x;
     --x;
   done

The loop body executes repeatedly until the condition is
not satisfied. If the condition is initially unsatisfied
the body is not executed. 

The `repeat` loops is an infinte loop equivalent to `while true`.

The `until` loop is a `while` loop with a negated condition.


C style for loop
----------------

Syntax:

.. code-block:: felix

  loop_stmt := optlabel "for" "(" stmt sexpr ";" stmt ")" stmt 
  loop_stmt := optlabel "for" stmt "while" sexpr ";" "next" stmt block 
  loop_stmt := optlabel "for" stmt "until" sexpr ";" "next" stmt block

The first two forms execute a statement once which generally assigns
a value to a control variable. The expression must be of type `bool`
and is checked to see if the loop should execute. The C form and the 
`while` form check the condition whilst the `until` form
checks the negated condition. The `next` statement is used 
to increment the control variable.



Integer For Loops
-----------------

Syntax:

.. code-block:: felix

  loop_stmt := optlabel "for" sname "in" sexpr "upto" sexpr block
  loop_stmt := optlabel "for" "var" sname ":" sexpr "in" sexpr "upto" sexpr block
  loop_stmt := optlabel "for" "var" sname "in" sexpr "upto" sexpr block
  loop_stmt := optlabel "for" sname "in" sexpr "downto" sexpr block
  loop_stmt := optlabel "for" "var" sname ":" sexpr "in" sexpr "downto" sexpr block
  loop_stmt := optlabel "for" "var" sname "in" sexpr "downto" sexpr block

These are low level for loops which operate over inclusive ranges.
These loops require an integral control variable.
The forms with `var` create the control variable, the other forms
require it already exist. The control variable is available after
these loops execute.

The block of these forms do not constitue a scope, the loops are
implemented with gotos. Therefore you can put labels inside the
blocks and goto them, and you can return from the current
procedure or function inside the block.


Parallel Loop
-------------

Syntax:

.. code-block:: felix

  loop_stmt := "pfor" sname "in" sexpr "upto" sexpr block

The `pfor` loop requires the body of the loop to behave
independently of other iterations. the range is split
into N parts and N pthreads are executed, each one
handling a subrange of the loop. The threads run in the
system thread pool. N is chosen by the system depending
on the thread pool size and/or number of available cores.

`pfor` loops *must not be nested*. The reason is that the `pfor`
loop uses the system thread pool. It is safe in general for jobs in the 
thread pool to enqueue jobs to the thread pool. It is also inefficient
because the thread pool already executes N threads concurrently,
where N is roughly equal to the number of processor core available.

It is not necessary to initialise the thread pool to use a `pfor`
loops, it will be done automatically. However since the thread
pool *is* used, it *must* be destroyed to terminate the program.


Generic Loops
-------------

Syntax:

.. code-block:: felix

  loop_stmt := optlabel "for" sname "in" sexpr block
  loop_stmt := optlabel "rfor" sname "in" sexpr block
  loop_stmt := optlabel "match" spattern "in" sexpr block =>#

The generic `for` requires an function named `iterator`.
You can provide it directly, or, you can provide any data structure
which has an iterator method (that is, a function named iterator
which accepts the data structure as an argument). The iterator will
usually be a yielding generator and it must return an option type
`opt[T]`.

The loops process the `Some x` values yielded until `None` is found.

The control variables goes out of scope at the end of the loop.

The `for` variant uses a goto to loop around. 

The `rfor` variant uses recursion instead. The recursion will be 
flattened to a goto loop if it is safe, otherwise `rfor` will create a frame
for every iteration.

The `match` variant sets more than one variable by decoding the argument
of the `Some` constructor.


Labelled loops
--------------

Most loops allow an optional label which is written with just a `:` suffix.
You cannot goto such a label. The label is a name for the loop.

Labelled loops support labeled `break`, `continue` and `redo` statements.

.. code-block:: felix

   doit: for var i in 1 upto 10 do
     if i == 5 continue doit;
     if i == 7 do
        ++i;
        redo doit;
     done
     if i == 9 break doit;
  done

The `continue` statement jumps to the start of the selected loop,
adjusting the control variable as usual before checking.

The `break` statement exits the selected loop immediately.

The `redo` statement restarts the body of the selected loop
without adjusting the control variable and without checking it.


