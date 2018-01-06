
Felix
========
Added on master.

[Master Documentation Index](http://felix-documentation-master.readthedocs.io/en/latest)

Added on test branch. Change.

Felix is an advanced high performance statically typed scripting language.

It is as easy to run a program as Python:

```
flx filename
```

"just works". But underneath it generates highly optimised machine
binaries which outperform all interpreters, bytecode compilers,
virtual machines, and most compiled languages including C.
Felix is an aggressive inliner which performs whole program
analysis.

Features:
----------

- generates highly optimised ISO C++
- advanced resource manager organises compilation and linkage
- often runs faster than C
- glueless binding to C and C++ libraries
- lightweight threads with channels
- asynchronous network I/O
- thread safe garbage collection
- strictly statically typed
- overloading
- strong functional subsystem
  * first order parametric polymorphism
  * polymorphism with constraints
  * multitype Haskell style type classes
  * type classes with real semantic specification
  * pattern matching
  * first class function, sum, and product types
- user definabled and inline extensible grammar
- builds on all platforms
- runs on all platforms
- open source FFAU (free for any use) licence

Bindings:
-----------

- Google RE2 based regexp processing built
  * syntax support for regular definitions
- bindings to Gnu GMP and Gnu GSL included
- SDL: Simple Direct Media Layer
  * SDL based platform independent GUI 


If you want to read more, check out the [Annotated Reference Manual](https://github.com/felix-lang/felix/raw/master/docs/felix-ref.pdf) 
and the [website documentation](http://felix-lang.org/share/src/web/documentation.fdoc).

