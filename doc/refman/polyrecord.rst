PolyRecords
===========

Polyrecords are an extension of a record type designed to support
row polymorphism.

Syntax
------

.. code-block:: felix

  //$ polyRecord type.
  satom := "(" srecord_mem_decl ("," srecord_mem_decl2)*  "|" stypeexpr ")" =># 
   "`(ast_polyrecord_type ,(cons _2 (map second _3)) ,_5)";

  //$ polyrecord value
  //$ record value (comma separated).
  satom := "(" rassign ("," rassign2 )* "|" sexpr ")"


Description
-----------

A polyrecord is an extension of the record concept which allows
a product type to be extended by adding new fields on the left:

.. code-block:: felix

  typedef D2 = (x:double, y:double);
  typedef D3 = (z:double | D2);

  var a = (x=1.1, y=2.2);
  var b = (z=5.1 | b);

The expression after the vertical bar in a polyrecord expression must
have tuple or record type. The compiler actually allows any type during
type checking, so in principle any value can be tagged with an
attribute. However the code generation has no support for representations
other than tuples or records.

The primary purpose of polyrecords is to support row polymorphism.

