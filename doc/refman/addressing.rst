Addressing
==========

Syntax
------

.. code-block:: felix

  //$ C dereference.
  x[srefr_pri] := "*" x[srefr_pri]

  //$ Deref primitive.
  //x[srefr_pri] := "_deref" x[srefr_pri] 

  //$ Operator new.
  x[srefr_pri] := "new" x[srefr_pri]

  //$ Felix pointer type and address of operator.
  x[sthename_pri] := "&" x[sthename_pri] 

  //$ Felix pointer type and address of operator.
  x[sthename_pri] := "_uniq" x[sthename_pri] 
  x[sthename_pri] := "_rref" x[sthename_pri]
  x[sthename_pri] := "&<" x[sthename_pri] 
  x[sthename_pri] := "_wref" x[sthename_pri]
  x[sthename_pri] := "&>" x[sthename_pri]


  //$ Felix address of operator.
  x[sthename_pri] := "label_address" sname 


  //$ C pointer type.
  x[sthename_pri] :=  "@" x[sthename_pri]

