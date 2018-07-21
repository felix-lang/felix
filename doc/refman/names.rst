Qualified Names
===============

Syntax
------

.. code-block:: felix

  //$ Qualified name.
  sreally_qualified_name := squalified_name "::" ssimple_name_parts 

  squalified_name := sreally_qualified_name 

  squalified_name := ssimple_name_parts 
    

  ssimple_name_parts := sname 
  ssimple_name_parts := sname "[" "]"
  ssimple_name_parts := sname "[" sexpr "]" 

  //$ Suffixed name (to name functions).
  ssuffixed_name := squalified_name "of" x[sthename_pri] 
  


