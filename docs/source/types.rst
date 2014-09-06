Types
=====

`Syntax <http://felix-lang.org/share/lib/grammar/type_decls.flxh>`_

Tuples
------

Tuple types are well known: a tuple is just a Cartesian Product
with components identified by position, starting at 0. 
The n-ary type combinator is infix ``*`` and the n-ary value
constructor is infix ``,``:

.. code-block:: felix
   
   val tup : int * string * double = 1, "Hello", 4.2;

The 0-ary tuple type is denoted ``1`` or ``unit``
with sole value ``()``:

.. code-block:: felix
   
   val u : unit = ();

There 1-array tuple of type ``T`` component value ``v`` is identified
with the type ``T`` and has value ``v``.

The individual components of a tuple may be accessed by a projection
function. Felix uses an integer literal to denote this function.

.. code-block:: felix
   
   var x = 1,"Hello";
   assert 0 x == 1; assert x.0 == 1;
   assert 1 x == "Hello"; assert x.1 == "Hello";

[There should be a way to name this function without application to
a tuple!]

A pointer to a tuple is also in itself a tuple, namely the
tuple of pointers to the individual components. This means
if a tuple is addressable, so are the components.

.. code-block:: felix
   
   var x = 1, "Hello";
   val px = &x;
   val pi = px.0; pi <-42;
   val ps = px.1; ps <-"World";
   assert x.0 == 42;
   assert x.1 == "World";

In particular note:

.. code-block:: felix
   
   var x = 1, "Hello";
   &x.0 <- 42;

because the precedences make the grouping ``(&x).0``.

You cannot take the address of a tuple component because
a projection of a value is a value.

Assignment to components of tuples stored in variables is supported
but only to one level, for general access you must take a pointer
and use the store-at-addres operator ``<-``.

Records
-------

A record is similar to a tuple except the components are 
named and considered unordered.


Structs
-------

TBD
Sums
----

TBD

union
^^^^^

TBD

enum
----

TBD

variant
-------

TBD

Array
-----

TBD

typedef
-------

TBD

typedef fun
^^^^^^^^^^^

TBD

typematch
---------

TBD

type sets
---------

TBD

Abstract types
--------------

TBD

