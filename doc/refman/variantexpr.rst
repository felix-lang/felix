Variant literals
================


.. code-block:: felix

  //$ Case tag literal.
  x[scase_literal_pri] := "case" sinteger 
  x[scase_literal_pri] := "`" sinteger 

  //$ Case value.
  x[scase_literal_pri] := "case" sinteger "of" x[ssum_pri] 
  x[scase_literal_pri] := "`" sinteger "of" x[ssum_pri] 
  x[scase_literal_pri] := "`" sinteger ":" x[ssum_pri] 

  spv_name := "case" sname
  spv_name := "`" sname 

  //$ Variant value.
  x[sthename_pri] := "#" spv_name
  x[sapplication_pri] := spv_name  x[>sapplication_pri] 

