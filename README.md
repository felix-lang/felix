
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

Compiler     |  Ack   |  Takfp
-------------|--------|-----------
Felix/clang  |  3.71  |  6.23
Clang/C++    |  3.95  |  6.29
Felix/gcc    |  2.34  |  6.60
Gcc/C++      |  2.25  |  6.25
Ocaml        |  2.93  |  8.41

### C and C++ embedding

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

### Overloading

Ad hoc polymorphism.

```
// overloads
fun f (x:double) => x +42.1;
fun f (x:int) =>  x + 1;
fun f (x:string) => x + "!";
```

### Simple Generics

Just don't give the argument type.

```
// generics
fun g (x) => f (f x);
println$ g 1, g "hello";
println$ _map f (1,"hello",2.0);
```

### Type Classes

A better way of overloading:

```
class Eq[T] {
  virtual fun == T * T -> T;
  fun != (x:T, y:T) => not (x == y);
}
instance Eq[int] {
  fun == : int * int -> int = "$1==$2"; //from C++
}
```

### Pattern matching

```
match x with
| Some x => println$ x; 
| None => println "NONE";
endmatch;
``` 

### Pointers

The only way to store a value. Felix has no references or
lvalues. Pointers are better.

```
var x = 1;
&x <- 2;
```


### Type System based on Algebra

Felix uses a category theoretic model
of types which includes products (tuples etc), coproducts 
(variants etc), exponentials (closures), recursion, and pointers,
_as well_ as providing both (unenforced) purely functional
(declarative) handling of inductive types and
and purely cofunctional (control flow) handling of
cofunctional types.


### Purely Functional Programming 

With _parametric polymorphism_, _higher order functions_,
_lexically scoped closures_, and _garbage collection_.
Mutation is still possible by use of _uniqueness typing_
which provides _move semantics_.

Felix has both C++ style _ad hoc polymorphism_ with
overloading and also Haskell style _type classes_,
(what would be called _concepts_ in C++).

```
var out = fold_left 
  (fun (acc:int) (elt:int) => acc + elt) 
  0
  ([1,2,3,4])
;
  
```

### Coroutines 

In Felix imperative programming is done with statements
and procedures, but procedures are a special case
of coroutines. Any unit procedure can be spawned
as a _fibre_ or _lightweight thread_ which communicates
with other fibres using _synchronous channels_.


### User Domain Specific Sub-Languages

The Felix grammar is part of the library.
The programmer can design new syntax as desired
using standard EBNF grammar rules, with action
codes written in R5RS Scheme which transform the
non-terminal arguments into arbitrary S-expressions.
These S-expressions are reduced to standard Felix AST terms.

Combining the syntax extension ability with library code
and C++ embedding allows definition of an _integrated_ Domain Specific
__sub__-language of Felix. 

### Regular Definition DSSL

The following 
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

### Chips and Circuits DSSL
This DSSL provides a syntactic model of coroutines as chips,
and the topology of channel connections between these chips
as circults. The code below is an advanced combination
of this model, pipelines, and uses the fixpoint operator
for recursion, to build a simple parser.

```
include "std/strings/recognisers";
include "std/control/chips";

open BaseChips;
open Recognisers;

device L = match_string "(";
device R = match_string ")";
device E = match_string "E";

// Grammar:
// p = epsilon
// p = (p)p
// s = pE
var p = fix (proc (q:iochip_t[Buffer,Buffer]) 
  (io: (
    inp: %<Buffer,
    out: %>Buffer
  )) ()
 {
   device y = 
     tryall_list ([
       epsilon[Buffer],
       L |-> q |-> R |-> q
     ])
   ;
   circuit
     wire io.inp to y.inp
     wire io.out to y.out
   endcircuit
});

device parens = p |-> E;

device sayresult = procedure (proc (x:Buffer) {
  println$ "Test: End pos=" + x.str; })
;

device tests = source_from_list (["(()(()))E", "E", "()E"]);
device toBuffer = function (fun (s:string)=> Buffer s);

#(tests |-> toBuffer |-> parens |-> sayresult);

```

### Graphics

Felix has a builtin library for GUIs based on SDL2:

![Felix the cat](/src/web/images/graphics_demo.jpg)

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


