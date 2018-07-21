Comparisons
+++++++++++

.. code-block:: felix

  x[scomparison_pri]:= x[>scomparison_pri] cmp x[>scomparison_pri] 
  x[scomparison_pri]:= x[>scomparison_pri] "not" cmp x[>scomparison_pri]

Equivalences
------------

.. code-block:: felix

  cmp := "==" 
  cmp := "!="
  cmp := "\ne"
  cmp := "\neq"

Partial Orders
--------------

.. code-block:: felix

  cmp := "\subset"
  cmp := "\supset" 
  cmp := "\subseteq" 
  cmp := "\subseteqq"
  cmp := "\supseteq" 
  cmp := "\supseteqq"

  cmp := "\nsubseteq"
  cmp := "\nsubseteqq"
  cmp := "\nsupseteq"
  cmp := "\nsupseteqq"

  cmp := "\subsetneq"
  cmp := "\subsetneqq"
  cmp := "\supsetneq"
  cmp := "\supsetneqq"

Total Orders
------------

.. code-block:: felix

  cmp := "<" 

  cmp := "\lt"
  cmp := "\lneq" 
  cmp := "\lneqq" 

  cmp := "<=" 
  cmp := "\le"
  cmp := "\leq"
  cmp := "\leqq" 

  cmp := ">"
  cmp := "\gt"
  cmp := "\gneq" 
  cmp := "\gneqq" 

  cmp := ">=" 
  cmp := "\ge" 
  cmp := "\geq"
  cmp := "\geqq"

  cmp := "\nless"
  cmp := "\nleq" 
  cmp := "\nleqq"
  cmp := "\ngtr"
  cmp := "\ngeq" 
  cmp := "\ngeqq"

The usual comparison operators are available along with TeX identifiers:

==================== ==================
operator             numeric semantics
==================== ==================
==, \\eq              equality
!=, \\ne              inequality

<, \\lt               less than
<=, \\le              less or equal
>, \\gt               greater than
>=, \\ge              greater or equal
==================== ==================

Set Operators
+++++++++++++

.. code-block:: felix

  cmp := "in" =># '(nos "\\in")';
  cmp := "\in" =># "(nos _1)";
  cmp := "\notin" =># '(nos _1)';
  cmp := "\owns" =># '(nos _1)';

  x[ssetunion_pri] := x[ssetunion_pri] "\cup" x[>ssetunion_pri] 
  x[ssetintersection_pri] := x[ssetintersection_pri] "\cap" x[>ssetintersection_pri]


