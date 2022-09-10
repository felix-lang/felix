
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

## Getting Started

### Prerequisites

* Python 3
* Ocaml 4.06.1 (only for source build)
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
. buildscript/macosxsetup.sh
make  
sudo make install # optional!
```

#### Building with Nix

On platforms supporting Nix, you can set up a build and runtime environment
by running:

```
git clone https://github.com/felix-lang/felix.git
cd felix
nix-shell shell.nix
. buildscript/linuxsetup.sh
make  
```

This will do an in place "install" of the Felix binaries. Note that
this should work on OS X with Nix, but needs to be tested. 


#### Windows

Follow the instructions on [the Wiki](https://github.com/felix-lang/felix/wiki/Building-Felix-From-Source#windows-10-with-visual-studio-2022).

## Packages

#### Arch Linux

Use provided [PKGBUILD](./src/misc/PKGBUILD) to make an installable package.
It is also available in the [AUR repository](https://aur.archlinux.org/packages/felix/)

```
cd src/misc
makepkg
sudo pacman -U felix-VERSION.pkg.tar.xz
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
Felix Wiki                           | <https://github.com/felix-lang/felix/wiki>
Git Repository                       | <https://github.com/felix-lang/felix>
Binary Download                      | <http://github.com/felix-lang/felix/releases>

# Mailing List

mailto:felixpl@googlegroups.com


# Licence

Felix is Free For Any Use (FFAU)/Public Domain.


