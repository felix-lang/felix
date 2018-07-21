Tuple Expressions
=================

Syntax
------

.. code-block:: felix

  //$ Tuple formation by cons: right associative.
  x[stuple_cons_pri] := x[>stuple_cons_pri] ",," x[stuple_cons_pri]

  //$ Tuple formation by append: left associative
  x[stuple_cons_pri] := x[stuple_cons_pri] "<,,>" x[>stuple_cons_pri] 

  //$ Tuple formation non-associative.
  x[stuple_pri] := x[>stuple_pri] ( "," x[>stuple_pri])+ 



