Varray
======

A varray is a variable length array with a construction time bounded maximum
length. Unlike ordinary arrays, varrays are mutable and passed by reference.
Underneath a varray is just a pointer.


Empty Varray
------------

An empty varray can be constructed by giving the bound, the bound
must be of type size:

.. code-block:: felix

  var x = varray[int] 42.size;

The type of the varray must be specified in this form.


Construction from container
---------------------------

A varray can be constructed from an ordinary array, list, or string,
and from an another varray with or without a bound specified:

.. code-block:: felix

  var v4 = varray (1,2,3,4);              // length 4, maxlen 4
  var v8 = varray (v4, 8.size);           // length 4, maxlen 8
  var y8 = varray (v8);                   // length 4, maxlen 4
  var y12 = varray (y8, 12.size);         // length 4, maxlen 12
  var z4 = varray ([1,2,3,4]);            // length 4, maxlen 4

  var s12 = varray "Hello World";         // trailing NUL included!

Construction from default value
-------------------------------

A varray can also be built to given size and filled with
a default value:

.. code-block:: felix

  var v100 = varray (100.size, char 0); // buffer
 

Length 
------

The length of a varray is given by the `len` function, the bound
is given by the `maxlen` function:

.. code-block:: felix

  var x = varray 42.size;
  println$ "len=" + x.len.str + ", maxlen=" + x.maxlen.str;


Extend and Contract
-------------------

A new element can be pushed at the end of a varray with the push_back procedure,
provided the resulting array doesn't exceed its maxlen bound. Similarly
an element can be removed from the end, provided the array isn't empty:

.. code-block:: felix

  var x = varray[int] 42.size;
  x.push_back 16; // length now 1
  x.pop_back;     // remove last element


Insert at position
------------------

An element can be inserted at a given position, provided the position
does not exceed the current length, and is greater than or equal
to the maxlen:

.. code-block:: felix

  var x = varray[int] 42.size;
  insert (x, 0, 42);
  insert (x, 0, 41);
  insert (x, 2, 42);

Erase elements
--------------

Elements can be erased by giving the position to erase, or,
an inclusive range:

.. code-block:: felix

   var x = varray (1,2,3,4,5,6);
   erase (x, 2);
   erase (x, 2, 20);

This procedure cannot fail. Attempts to erase off the ends of the array
are simply ignored.

Get an element
--------------

An element can be fetched with the get function, provided the index is 
in range:

.. code-block:: felix

   var x = varray (1,2,3,4,5,6);
   println$ get (x, 3.size); // 4
   println$ x.3;             // 4 

The last form allows any integer type to index a varray.

Set an element
--------------

An element can be modified with the set procedure:

.. code-block:: felix

   var x = varray (1,2,3,4,5,6);
   set (x, 3.size, 99); // 4 changed to 99




