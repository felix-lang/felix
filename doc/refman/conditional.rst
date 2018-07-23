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



