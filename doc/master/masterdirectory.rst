==========================
Felix documentation Master
==========================

This is the master document for Felix documentation.
It provides some general description of Felix, along
with links to more specific documentation.

Specific Documents
==================

* Documentation Master (this document) <http://felix-documentation-master.readthedocs.io/en/latest/>
* Installation and Tools Guide <http://felix-tools.readthedocs.io/en/latest/>
* Felix Language Reference Manual <http://felix.readthedocs.io/en/latest/>
* Felix Tutorial <http://felix-tutorial.readthedocs.io/en/latest/>
* Felix Library Packages <http://felix-library-packages.readthedocs.io/en/latest/>
* Articles on Modern Computing <http://modern-computing.readthedocs.io/en/latest/>
* Felix Home Page <http://felix-lang.org>
* Git Repository <https://github.com/felix-lang/felix>
 
General Description
===================

Felix is a high level statically typed programming language 
designed with several key features in mind.

lightspeed performance
----------------------

Which means, as fast as C, if not faster

C and C++ ABI compatibility
---------------------------

the ability to embed exising C and C++ code


Ease of use
-----------

as easy to use as a scripting language`, which means no make files or switches
for basic operation

high reliability
----------------

which means a fully statically typed language, for which
reasoning about correctness is well supported

programmers toolkit
-------------------

which means we provide many useful features
and libraries, with multiple ways to combine and use them according
to the application requirements and programmers taste

flexible deployment
-------------------

which means the system can be used both as
a personal development system, as well as for enterprise level team
projects

write once run anywhere
-----------------------

the same code working the same way on all platforms

Language Design Goals
=====================

The Felix language has a number of important design goals.

* full integration of `coroutines` as core control structures

* full support for functional programming including 
  # parametric polymorphism, 
  # Haskell style type classes
  # a wide range of type constructors including 
     + tuples
     + arrays
     + records
     + structs
     + anonymous sums
     + traditional nominally typed variants
     + generalised algebraic types (GADTs)
     + polymorphic variants
     + subtyping
     + uniqueness types
     + row polymorphism for records
     + first class functions and procedures
     + pointers
     + first class projections and injections

* expanded products: no boxing
* garbage collection
* algol like imperative programming as a subset of the coroutine system
* Java like objects and interfaces
* dynamically loadable plugins
* asynchronous I/O support
* pre-emptive threading support
* user defined grammar
* LaTeX/AMSTeX symbol set

 
