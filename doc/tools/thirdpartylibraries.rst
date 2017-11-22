===========================
Using Third Party Libraries
===========================

Felix is specifically designed to be able to use third party
C and C++ libraries, and to be able to do so without any
makefiles, linkage commands, or other scripts.

To make this happen, there is a configuration database
which tells Felix where the relevant header files and
libraries actually reside in your file system.

In fact, Felix itself uses the very same machinery
to use its own native libraries.

Targets
=======

Felix can build for any targets you can configure.
The default, required, target is called `host`,
and it is your normal programming environment.

Felix splits its data into two parts, `shared` and
the target, usually `host`. The data in `shared` is
common to all targets; it consists of all platform
independent data, including Felix source libraries,
and platform independent C++ header files.

In the target `host` you will find object files and
executables compiled for your specific operating system
by the C++ compiler toolchain selected as the default.

Configuration Database
======================

Inside every platform dependent target, there is a
directory called `config`, you can examine it here
from the Felix install directory:

.. code-block:: bash

    ls build/release/host/config

You will see a bunch of files ending in extension `.fpc`.
Each of these files specifies how to use a package with
the basename of the file. Lets look at one:

.. code-block:: text

    ~/felix>cat build/release/host/config/sdl2.fpc
    Generated_from: 3674 "/Users/skaller/felix/src/packages/sdl.fdoc"
    Name: SDL2
    Description: Simple Direct Media Layer 2.0
    cflags: -I/usr/local/include/SDL2
    includes: '"SDL.h"'
    provides_dlib: -L/usr/local/lib -lSDL2
    provides_slib: -L/usr/local/lib -lSDL2
    requires_dlibs: ---framework=OpenGL
    requires_slibs: ---framework=OpenGL

This is the one on my Mac, which runs OSX. The first line tells
how the file got produced, the second is a common name for
the library which is just documentation too, as is the third
line, which gives more information.

But now come the important lines.

* `cflags` tells the C++ compiler how to find the SDL header file
* `includes` tells Felix what the header file name is
* `provides_dlib` tells C++ how to link the dynamic version of the SDL2 library
* `provides_slib` tells C++ how to link the static version of the SDL2 library
* `requires_dlibs` tells C++ about dependencies of SDL2 dynamic library
* `requires_slibs` tells C++ about dependencies of SDL2 dynamic library

Note that values of these attributes are specific to *your* computer,
and also sometimes to the C++ compiler selected.

When you install a third party library, you have to create an entry
similar to that seen above for the library in the configuration
database for each target where you want to use that library.

On Ubuntu, you will normally only need three lines

.. code-block:: text

    includes: '"mylib"'
    provides_dlib: -lmylib
    provides_slib: -lmylib

because C++ will automatically look in `/usr/lib` for libraries,
and in `/usr/include` for header files. Do not include the
leading `lib` of library file names, nor the trailing `.so`!


Felix Source Code
=================

Now you have installed your library, and added entries
in the configuration database, you can use the library like this:

.. code-block:: elix

    type mytype = "mylib::mytype"
      requires package "mylib"
    ;


This Felix code lifts the C++ type `mylib::mytype`
into Felix, naming it `mytype` in Felix, and it tells
Felix that the library providing that C++ types is 
in package `mylib` .. which of course is just the name
of the file `mylib.fpc` in the configuration database.

This is how Felix maps abstract component name used
in source code into the compilation and linker switches
needed to use the C++ and binary library components.

The source code is therefore platform independent,
and you can run programs which uses third party
libraries without specifying any linker switches
or other platform specific nonsense every again.
You have to specify it once, in the configuration
database.

Felix is smart, it will only link the library if it
is actually required. If you do not use values of the
type `mytype` then the library will not be linked,
if you do, it will be.





