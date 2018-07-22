Variant literals
================


.. code-block:: felix

  //$ Case value, sum types
  x[scase_literal_pri] := "case" sinteger "of" x[ssum_pri] 
  x[scase_literal_pri] := "`" sinteger "of" x[ssum_pri] 
  x[scase_literal_pri] := "`" sinteger ":" x[ssum_pri] 

  //$ Variant value, polymorphic variant type
  x[sthename_pri] := "#" "case" sname
  x[sthename_pri] := "#" "`" sname
  x[sapplication_pri] := "case" sname x[>sapplication_pri] 
  x[sapplication_pri] := "`" sname x[>sapplication_pri] 

  //$ Variant decode.
  x[sapplication_pri] := "caseno" x[>sapplication_pri]
  x[sapplication_pri] := "casearg" x[>sapplication_pri] 


