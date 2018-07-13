Primitive Types
===============

Felix provides almost no primitive types.
Instead, it provides two orthogonal facilities.

First, it provides a way to lift, or bind, types from C or C++.
Second, it provides type constructors to manufacture new types
from other types.

Type Bindings
-------------

Any first class type defined in C++ can be lifted into
Felix with a type binding statement:

.. code-block:: felix

   type int = "int";

   type string = "::std::basic_string<char>"
      requires Cxx_headers::string
   ;

   type vector[T] = "::std::vector<?1>"
     requires Cxx_headers::vector
   ;

   type fred = "fred" 
     requires header """
       struct fred { int x; };
     """
   ;


Templates with type parameters can be lifted by using
the special encoding of a question mark followed by a 
single digit which refers to the n'th type parameter
in the Felix type: the name given in the Felix type 
is a dummy and not used.

A type binding may provide a requires clause which specifies
a floaing insertion which ensures the C++ code generated
by the compiler inserts the required definition of the C++
type. Floating insertions are described in detail elsewhere.

Floating insertions which inject #includes for all C and C++
standard headers, and all Posix headers are provided in the library, 
split according to the ISO standard version.

The code of an insertion is emitted at most once, depending
on whether the type is used or not.

Distinct Types
--------------

In the Felix standard library, most named typed from C, C++,
and Posix, are distinct types, even if they're aliases in C/C++.
This can be particularly annoying however the choice ensures
proper separation of overloads based on these types, independently
of platform dependent aliasing in C.

