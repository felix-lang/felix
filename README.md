
# Felix

An advanced, statically typed, high performance scripting language with native C++ embedding.

## Features
### Autobuilder
This file:
```
// hello.flx
println$ "Hello World";
```

can be run directly:

```bash
flx hello.flx
```

It __just works__. No makefiles. No compiler switches.
Automatic caching and dependency checking for Felix and C++.
Uses a *flx_pkgconfig* database consisting of a directory
of `*.fpc` files to specify and find libraries and header
files based on in language abstract keys.

### Hyperlight Performance

The aim is to run faster than C.

Underneath Felix generates highly optimised machine
binaries which outperform most interpreters, bytecode compilers,
virtual machines, and sometimes compiled languages including C and C++.

Felix is an aggressive inliner which performs whole program
analysis and high level optimisations such as parallel assignment,
self-tail call elimination.

Felix generates optimised C++ which is then compiled and optimised
again by your system C++ compiler.

## C and C++ embedding

Felix is a C++ code generator specifically designed so that 
all your favourite C and C++ libraries can be embedded
with little or no glue logic:

```
// required header 
header vector_h = '#include <vector>';

// C++11 for smart pointers
header memory_h = '#include <memory>' 
  requires package "cplusplus_11"
;
 
// types
type vector[T] = "::std::shared_ptr<::std::vector<?1>>" 
  requires vector_h, memory_h
;

type viterator[T] = "::std::vector<?1>::iterator"
  requires vector_h
;

// constructor
ctor[T] vector[T] : unit = "::std::make_shared<::std::vector<?1>>()";

// operations
proc push_back[T] : vector[T] * T =  "$1->push_back($2);";
proc push_back[T] (v: vector[T]) (elt:T) => push_back(v,elt);

fun stl_begin[T] : vector[T] -> viterator[T] = "$1->begin()";
fun deref[T] : viterator[T] -> T = "*$1";

// example use
var v = vector[int]();
v.push_back 42;
println$ *v.stl_begin;

```

### Type System based on Algebra
In particular, Felix uses a category theoretic model
of types which includes products (tuples etc), coproducts 
(variants etc), exponentials (closures), recursion, and pointers,
_as well_ as providing both (unenforced) purely functional
(declarative) handling of inductive types and
and purely cofunctional (control flow) handling of
cofunctional types.


### Purely Functional Programming 
With _parametric polymorphism_, _higher order functions_,
_lexically scoped closures_, and _garbage collection_.

Felix has both C++ style _ad hoc polymorphism_ with
overloading and also Haskell style _type classes_.

Functional programming uses _recursion_ to decode
inductive data types, especially _lists_, a degenerate
form of _trees_.

### Purely Cofunctional Programming
In Felix imperative programming is done with statements
and procedures, but procedures are a special case
of coroutines. Any unit procedure can be spawned
as a _fibre_ or _lighweight thread_ which communicates
with other fibres using _synchronous channels_.

Coroutines which depend only on channel I/O and
have no side-effects are pure, __dual__ to the notion
of a pure function.

Coroutines use _iteration_ to decode coinductive
data types, especially _streams_, a degenerate
form of _control flow graphs_.

### Duality Bridges
Felix provides duality bridges. Pointers in the imperative
model are dual to recursion in the functional model.
Infinte loops and continuation passing via channels in coroutines 
are dual to recursion in the functional model.

Felix provides bridges to lift functional code to the
cofunctional model and drop it back again, so you can
program with both functions and continuations and switch
models. For example the function chain:

```
f . g . h 
```
can be lifted to a pipeline:

```
function f |-> function g |-> function h 
```

equivalent to a state monad: the pipeline processes
an infinite stream of values, whereas the function
composition has to be mapped over a finite list tail.

### User Domain Specific Sub-Languages

The Felix grammar is part of the library.
The programmer can design new syntax as desired
using standard EBNF grammar rules, with action
codes written in R5RS Scheme which transform the
non-terminal arguments into arbitrary S-expressions.
These S-expressions are reduced to standard Felix AST terms.

Combining the syntax extension ability with library code
and C++ embedding allows definition of an _integrated_ Domain Specific
__sub__-language of Felix. For example the following 
code uses library _combinators_ and embedded Google RE2
binding to specify a C identifier (with only `x` for letters
and `9` for digits):

```
var digit = Charset "9";
var letter = Charset "x";
var us = Regdef::String "_";
var id = Seqs 
  ([   
     Alts ([us,letter]),
     Rpt( Alts([letter,digit,us]), 0,-1)
  )]
;
```
With the standard regexp grammar we can generate the
combinators and thus calls to Google RE2 using 
the regexp DSSL:

```
regdef digit = "9";
regdef letter = "x";
regdef us = "_";
regdef id = (us|letter)(letter|digit|us)*;
```
which is much better than the string form:
```
(?:\x5F|[x])(?:[x]|[9]|\x5F)*
```

## Getting Started

### Prerequisites

* Python 3
* Ocaml 6.01 (only for source build)
* C++ compiler: g++, clang++, or msvc

### Extras (can be installed later)

* SDL2 for graphics
* GNU GMP, GNU GSL 

### Build from Source

#### Linux

```
git clone https://github.com/felix-lang/felix.git
cd felix
. buildscript/linuxsetup.sh
make  
sudo make install # optional!
```

#### OSX


```
git clone https://github.com/felix-lang/felix.git
cd felix
. buildscript/osxsetup.sh
make  
sudo make install # optional!
```

#### Windows
Make sure git, Python3 and Ocaml are on your PATH.
You can download a pre-built [Ocaml 6.01 for Windows](https://github.com/felix-lang/win64ocaml).

Open a cmd.exe console with Visual Studio 2015 or above
environment established or run vcvarsall x86. See [vcvarsall](https://msdn.microsoft.com/en-us/library/f2ccy3wt.aspx).

```
git clone https://github.com/felix-lang/felix.git
cd felix
. buildscript/winsetup.sh
nmake  
nmake install # optional!
```

## Tarballs

<http://github.com/felix-lang/felix/releases>

# Build Status

Appveyor, Windows build: [![Build Status](https://ci.appveyor.com/api/projects/status/q9w45r6b2chnsre1?svg=true)](https://ci.appveyor.com/project/skaller/felix)
Travis, Linux build: [![Build Status](https://travis-ci.org/felix-lang/felix.svg?branch=master)](https://travis-ci.org/felix-lang/felix)

# Links 

Title                                | URL
------------------------------------ | ------------------------------------------------------------
Documentation Master                 | <http://felix-documentation-master.readthedocs.io/en/latest/>
Felix Tutorial                       | <http://felix-tutorial.readthedocs.io/en/latest/>
Installation and Tools Guide         | <http://felix-tools.readthedocs.io/en/latest/>
Felix Language Reference Manual      | <http://felix.readthedocs.io/en/latest/>
Felix Library Packages               | <http://felix-library-packages.readthedocs.io/en/latest/>
Articles on Modern Computing         | <http://modern-computing.readthedocs.io/en/latest/>
Felix Home Page                      | <http://felix-lang.github.io/felix>
Git Repository                       | <https://github.com/felix-lang/felix>
Binary Download                      | <http://github.com/felix-lang/felix/releases>

# Mailing List

mailto:felix-lang@googlegroups.com


# Licence

Felix is Free For Any Use (FFAU)/Public Domain.


![Felix the cat](/src/web/images/FelixWork.jpg)
