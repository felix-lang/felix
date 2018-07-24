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

  //$ Named temporary value.
  x[sas_expr_pri] := x[sas_expr_pri] "as" sname 


  //$ Named variable.
  x[sas_expr_pri] := x[sas_expr_pri] "as" "var" sname 


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

The `as` expressions allow a `val` or `var` to be defined inside an expression.
The definition is lifted out of the expression and replaced by the named variable.
This program:

.. code-block:: felix

  var y = (1 as x) + 10;
  var z = y + 100;
  println$ x + y + z;
 
is equivalent to:

.. code-block:: felix

  var x = 1;
  var y = x + 10;
  var z = y + 100;

  println$ x + y + z;
  
