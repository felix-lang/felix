Let Forms
=========

Syntax
------

.. code-block:: felix

  x[let_pri] := "let" spattern "=" x[let_pri] "in" x[let_pri] 
  x[let_pri] := "let" spattern "=" x[let_pri] "in" x[let_pri] 
  x[let_pri] := "let" "fun" sdeclname fun_return_type "=" smatching+ "in" x[let_pri]
  x[let_pri] := "let" pattern_match 
  x[let_pri] := pattern_match 

Description
-----------

A let form allows an expression to be factored:

.. code-block:: felix

  let p = expr1 in expr2

for example:

.. code-block:: felix

  let x2 = x * x in
  let y2 = y * y in
    sqrt (x2 + y2)

Another let form defines a local function:

.. code-block:: felix

  let fun sq(x:int) = x * x in 
    sqrt (sq x + sq y)

