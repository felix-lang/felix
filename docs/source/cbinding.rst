C bindings
==========

Felix is specifically designed to provide almost seamless integration
with C and C++.

In particular, Felix and C++ can share types and functions,
typically without executable glue.

However Felix has a stronger and stricter type system than C++
and a much better syntax, so binding specifications which lift
C++ entities into Felix typically require some static glue.

Type bindings
-------------

In general, Felix requires all primitive types to be first class,
that is, they must be default initialisable, copy constructible,
assignable, and destructible. Assignment to a default initialised
variable must have the same semantics as copy construction.

It is recommended C++ objects provide move constructors as
Felix generated code uses pass by value extensively.

The Felix type system does not support C++ references in general,
you should use pointers instead. 

However, there is a special lvalue annotation for C++ functions
returning lvalues that allows them to appear on the LHS of
an assignment. Only primitives can be marked lvalue.

The Felix type system does not support either const or volatile.
This has no impact when passing arguments to C++ functions.
However it may be necessary to cast a pointer returned from
a primitive function in order for the generated code to type check.



Expression bindings
-------------------

TBD

Function bindings
-----------------

TBD

Floating insertions
-------------------

TBD

Package requirements
--------------------

TBD

