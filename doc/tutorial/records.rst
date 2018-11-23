Record
======

A record is like a tuple, except the components are named:

.. code-block:: felix

  var x = (a=1,b="hello", c=42.0);
  println$ x.b;

Actually, you can use a blank name, or leave a name out:

.. code-block:: felix

  var x = (a=1,42.0,n""="What?");

Note the use of the special identifier form `n"..."` in which the text of
the identifier is zero length.

Duplicate Fields
----------------

Fields names in a record can be duplicated:

.. code-block:: felix

  var x = (a=1,a=2,32,77);

In this case, when the field name is used to access a component
it refers to the left most instance of the field. While this may
seem like an unusual feature in isolation, it is needed to support
polyrecords (row polymorphism).

There is a special case: if all the field names are blank,
the the record is a tuple. So in fact tuples are just a special
case of records.

Function Application
--------------------

Earlier we saw examples of :ref:`function application <functions-101>`, but
function application is implicitly performed on tuples and records:

.. code-block:: felix

  fun f(x:int,y:double)
  // accepts either of the following
  f (1,2.1)
  f (x=1,y=2.1)

The order doesn’t matter if you use names, except for duplicates.

  
  





  

