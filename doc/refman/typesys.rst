
.. felix documentation master file

Type System
===========

Felix is a statically typed scripting language which has a rich and
powerful type system. It supports type deduction and both ad-hoc
and type class based overloading, but not type inference. It provides
standard parametric polymorphism with ad-hoc weak constraints.

Types are represented with three distinct but inter-related mechanisms:

* Compiler Intrinsics
* User defined nominal types
* General Lifted Types
* Types lifted from the RTL
* Types modelled with type classes algebras

Compiler intrinsics
-------------------

Most compiler intrinsics are structural type constructors, for example
tuple and record constructors.

User defined types
------------------

Most user defined types are nominal types where user definitions
tell the comopiler how to construct and use a named type. Struct and
union constructors are examples of such types. The types are defined
in user space by the algebraic properties are intrinsic. For example
struct field names are intrinsically known to be projection functions.

General Lifted Types
--------------------

Most primitives types in Felix are lifted from C++ by use
of primitive type, function, and procedure bindings.
These types are opaque, or abstract, in the sense that
the compiler has no knowledge of the type representation.


RTL Types
---------

Some types are lifted from the Run Time Library,
using the primitive binding machinery. The compiler
typically has no knowledge of these types however
they may be designed to interoperate with compiler
intrinsics.

Algebraically specified Types
-----------------------------

Many types are equipped with algebras by the use
of type classes. These are typically families
of specific sets of nominal types which are related
by sharing a common algebra.

Algebras
--------

Felix provides three forms of algebraic modelling.

First, the compiler provides certain intrinsic algebras
for structural types such as tuples, arrays, records,
sums, polymorphic variants, pointers.

Second, the compiler provides instrinsic algebras for
user defined nominal types such as structs, cstructs, unions,
functions, generators and procedures.

Third, the compiler provides machinery, primarily type classes,
primitive bindings, and user defined syntax,
for the user to define their own algebras .


Contents:

.. toctree::
   :maxdepth: 1
   
   kinds
   typealias
   typefun
   typematch
   typeset
   typecase
   primtypes
   abstractyp
   tuples
   records
   structs
   pointers
   classes
   unions
   sums
   variants
   compactlinear
   funtyp
   subtyping
   overloading
   typeclass
   abstract

