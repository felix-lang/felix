Pointer Types
=============

Machine Pointers
----------------

A machine pointed is a typed machine address.
Felix has three types of machine level pointers.

==== ===== ======= ============================
Type Deref Storeat Semantics
==== ===== ======= ============================
&<T  Y     N       Read only pointer to T
&>T  N     Y       Write only pointer to T
&T   Y     Y       Read or write pointer to T
==== ===== ======= ============================

Read only pointers are semantically equivalent to
C++ const pointers.

Operations
~~~~~~~~~~

Dereference
^^^^^^^^^^^

The system builtin dereference operation is 
named :code:`_deref` and is an operator equivalent
to a function

.. code-block:: felix

  fun _deref: &<T -> T;

The library defines an overloadable function of the same
type:

.. code-block:: felix

  fun deref[T] (p:&<T): T => _deref p;


which can also be invoked by the syntax:

.. code-block:: felix

  *p

The dereference operation fetches the value of a storage location
at the address of the pointer.

Storeat
^^^^^^^

The system buildin storage operation is named :code:`_storeat`
and is equivalent to a procedure

.. code-block:: felix

  proc _storeat: &>T * T;

The library defines an overloadable procedure of the
same type:

.. code-block:: felix

  proc storeat[T] (p:&>T, v:T) => _storeat(p,v);

which can also be invoked by the syntax

.. code-block:: felix

  p <- v;

The storeat operation stores its value argument into
the storage location at the address of the pointer.

Subtyping Rules
~~~~~~~~~~~~~~~

The read-write pointer type is an invariant subtype of 
the read-only pointer type, and an invariant subtype
of the write only pointer type.

In theory read is covariant and write contravariant but
this is not implemented at the present time.

This means a read-write pointer may be used wherever
either a read-only or write-only pointer is required.

Constructors
~~~~~~~~~~~~

All three types of pointers can be constructed by addressing
a variable.

.. code-block:: felix

   var x = 1; // type int
   var ropx : &<int = &<x;
   var wopx : &>int = &>x;
   var rwpx : &int = &x;

In addition, a read-write pointer is returned by the system
intrinice operator :code:`new` which copies a value onto the
heap and returns a pointer to it:

.. code-block:: felix

  var px = new 42; // &int

Other operations returning pointers are defined in the library,
typically by binding to C or C++ functions such as :code:`malloc`.

Pointer Projections
~~~~~~~~~~~~~~~~~~~

Projection operators applying to arrays, tuples, records,
and structs, are all overloaded to work on pointers 
to these types. For example, to store a value in a structure
component:

.. code-block:: felix

  struct X { int a; int b; };
  var x = X (1,2);
  &x . a <- 42; // sets x.a to 42


