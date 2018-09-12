
# Felix

An advanced, statically typed, high performance scripting language with native C++ embedding.

```
println$ "Hello World";
```

It is as easy to run a program as Python:

```
flx hello.flx
```

__just works__. No makefiles. No compiler switches.

Underneath it generates highly optimised machine
binaries which outperform all interpreters, bytecode compilers,
virtual machines, and most compiled languages including C.
Felix is an aggressive inliner which performs whole program
analysis.

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
