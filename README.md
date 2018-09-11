
Felix
=====


[![Build Status](https://travis-ci.org/felix-lang/felix.svg?branch=master)](https://travis-ci.org/felix-lang/felix)


[Master Documentation Index](http://felix-documentation-master.readthedocs.io/en/latest)

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

Building
--------

To build from source first ensure you have Python 3.4 or above, Ocaml 3.04 or above native code compilers, and gcc, clang, or MSVC++ installed.  Open a terminal window (CMD.EXE on Windows) and set up the requisite environment to run the above language translators. You will of course also need the usual system utilities.

Optionally install SDL2, SDL_image and SDL_ttf if you want SDL based graphics support.

Clone the Felix git repository and type "cd felix".

Set up the environment for bootstrapping by ". buildscript/linuxsetup.sh" on Linux, ". buildscript/osxsetup.sh" on OSX, or "buildscript/windowsetup.bat" on Windows.

Now just type "make".

To install, you can type "sudo make install" on unix like systems, or just "make install" on Windows.

It is recommended, however, you do NOT install Felix but run it directly from inside the repository, since this makes updating easier. To make this work, extend your PATH and LD_LIBRARY_PATH or DYLD_LIBRARY_PATH as indicated in the buildscript you used. Then add the file "$HOME/.felix/config/felix.fpc" on Unix systems or "$USERPROFILE/.felix/felix.fpc" on Windows systems with the following line "FELIX_INSTALL_DIR=repositorydir/build/release" replacing repositorydir with the toplevel directoy of the repository image.

To test, type "flx hello.flx".

