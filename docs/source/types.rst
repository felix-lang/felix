Types
=====

`Syntax <http://felix-lang.org/share/lib/grammar/type_decls.flxh>`_

Tuples
------

Tuple types are a well known structural type: 
a tuple is just a Cartesian Product
with components identified by position, starting at 0. 
The n-ary type combinator is infix ``*`` and the n-ary value
constructor is infix ``,``:

.. code-block:: felix
   
   val tup : int * string * double = 1, "Hello", 4.2;

The 0-ary tuple type is denoted ``1`` or ``unit``
with sole value ``()``:

.. code-block:: felix
   
   val u : unit = ();

The 1-array tuple of type ``T`` component value ``v`` is identified
with the type ``T`` and has value ``v``.

The individual components of a tuple may be accessed by a projection
function. Felix uses an integer literal to denote this function.

.. code-block:: felix
   
   var x = 1,"Hello";
   assert 0 x == 1; assert x.0 == 1;
   assert 1 x == "Hello"; assert x.1 == "Hello";

[There should be a way to name this function without application to
a tuple!]

Integer literals can be used as projections with pointers
to tuples, the result is a pointer to the subcomponent.

This means if a tuple is addressable, so are the components.

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

Records are structurally typed analogous to anonymous structs.

A record type specified by enlosing comma separated list of 
record type component specifiers in parentheses.

A record type component specifier consists of an identifier
followed by a colon ``:`` followed by a type (of higher precedence
that comma ``,``).

A record value is specified by enclosing a comma separated list of
record component specifiers.

A record component specifier consists of an identifier followed by
an equal sign ``=`` followed by an expression (of higher precedence
than comman ``,``).

.. code-block:: felix

   var r : (a:int, b:string) = (a=1, b="hello"); 

A record type may be s

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

