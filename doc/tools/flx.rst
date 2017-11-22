============
The flx tool
============

The executable `flx` is the primary tool used to interface components
of Felix for the purpose of building, testing, and running Felix programs.

To work, `flx` must be on your `$PATH` or must be invoked using an absolute
or relative pathname. 

It is your responsibility to choose how your OS finds the `flx` program.

Once `flx` starts, it should be able to find everything else it needs
automatically. By default, it uses the hard coded installation directory
to find things, which is `/usr/local/lib/felix/felix-version` on Unix
systems or `C:\usr\local\lib\felix\felix-version` on Windows, where
`felix-version` is the version of Felix installed.

You can override this hard coded default in several ways. The best
way is to create a configuration file named:

.. code-block:: text

  $HOME/.felix/config/felix.fpc

on Unix systems or

.. code-block:: text

  $USERPROFILE\.felix\config\felix.fpc

on Windows. This file just needs to specify the installation directory:

.. code-block:: text

    FLX_INSTALL_DIR=/home/users/me/work/felix

You can also use an environment variable with the same name.

The `flx` tool needs to know the location of a lot of components,
but it will derived most of these correctly from the single location
given by the `FLX_INSTALL_DIR`.

If you build Felix from the git repository, then from the top
level of the repository the installation directory is `build/release`
on Unix or `build/release` on Windows. That is a relative file name,
so you should set the variable to the corresponding absolute
filename, which would be given by 

.. code-block:: bash 

   echo $PWD/build/release

on Unix.

Basic Usage
===========

Once it is all set up, you should be able to run felix programs like this:

.. code-block:: bash

    flx hello.flx


This should work no matter what you current directory is, no matter where
the target file is, and it should not write anything important anywhere
except in $HOME/.felix/cache. You may find a couple of files like

.. code-block:: text

   flxg_stats.txt

floating about, that just contains some performance stats for the compiler,
you can delete it freely, or complain so I can stop poluting your system.

Stand alone executable
======================

To make a stand alone executable you should do this:

.. code-block:: bash

    flx --static -c -od . hello.flx

and you will find the executable afterwards in the current directory.
It will be named `hello` on Unix like systems or `hello.exe` on Windows.
You can then run it as usual by `./hello` on Unix, or just `hello` on Windows.

The `--static` switch tells Felix to use static linkage, which is required
for a standalone executable. Normally Felix uses dynamic linkage.

The `-c` switch tells Felix not to run the program.

The `-od .` means to set the output directory to the current directory `.`.

You can also use `-ox myhello` to set the output pathname to `myhello`
without specifying the extension. If you want to specify the extension
you can use `-o myhello.exe` for example. The reason not to do this
should be clear, it makes the command OS dependent.

The reason for the `-od` switch is more subtle: `flx` has a batch mode
which can a whole set of programs based on a regular expression. In this
case you don't know the name of the file to output, since it is determined
by matching the regular expression against a whole directory of felix
programs. When you build Felix, the batch mode is used to run all the
regression tests.

Compiling Felix with added C++
==============================

Felix is specifically designed to work with C and C++.
To this end, you can write Felix programs which requires
C++ code you also supply. 

It is not the purpose of this document to describe how to
embed C++ into Felix. However let us assume you have Felix
code which depends on the C++ file `mycxx.cpp`. You can then
use this command to compile the C++ as well as the Felix code:

.. code-block:: bash

    flx mycxx.cpp myfelix.flx

This will compile the mycxx.cpp file using the same C++ compiler
Felix uses, generate C++ for myfelix.flx, and compile than,
and link the compiled object files together and run them.

Felix does dependency checking on the C++ file.
So it will not recompile the file if you do not change it.

Compiling Felix with added object files
=======================================

Sometimes you want to compile C++ code to object files yourself.
In this case you can just add the object files to the command line.
On x86_64 platforms in particular you need to take care that you
compile the file for the same operational model as you will use
with Felix. With static linkage, you can then run your program like:

.. code-block:: bash

    flx --static mycxx.o myfelix.flx

On Windows, object files have the extension `obj` instead.
If you leave out the `--static` switch like this:

.. code-block:: bash

    flx mycxx.o myfelix.flx


you need to be sure you have compiled for relocatable code.
With g++ you may need the `-fPIC` switch on Linux.
So-called position independent code (PIC) is slower than
position dependent code due to the ABI used by Linux,
together with the x86_64 architecture. This problem may
or may not arise on other platforms. Felix is very careful
to distinguish object files generated for static linkage
and those for dynamic linkage. When in doubt, use `-fPIC`
because such code can usually also be statically linked.


Compiling Felix with added libraries
====================================

You can also tell Felix to link extra libraries into
your program. The easiest way is to just put the filename
of the library on the command line. Make sure you compile
with the right model! 

This method of linkage always works for static linkage:

.. code-block:: bash

    flx --static libmylib.a myfelix.flx

should link your program against the give static link archive
on Linux. On Windows you would use:

.. code-block:: bash

    flx --static mylib.lib myfelix.flx

If you compile in dynamic mode, you can also give
library names like this, they will just be passed
as written to the C++ compiler. This is definitely
NOT recommended because it probablyt will not work.

A better way is to pass specific linker switches:

.. code-block:: bash

    flx --static -Llibdir -lmylib.a myfelix.flx

This should work for both dynamic and static linkage.
On Unix, the switches shown are just passed directly
to the C++ compiler in link mode.

On Windows, the toolchain drivers use ths *same* switches,
but attempt to translate for MSVC++. For example:


.. code-block:: bash

    flx --static -Llibdir -lmylib.lib myfelix.flx

should work on Windows. Note that on Unix, the system
will look for libmylib.a whereas on Windows, it will
look for just mylib.lib, without the `lib` prefix.
MSVC++ uses different switches than Unix, but the toolchain
knows what `-L` and `-l` mean and map these switches
over to MSVC++ syntax.

Using specific switches like this is not recommended
except briefly for experimentation. It is much better
to register the library in the configuration database.


Compiling C++ only
==================

flx can compile and run C++ programs, programs witten
entirely without any Felix. For example:


.. code-block:: bash

    flx --c++ --static needed.cpp mainline.cxx
 
All you need is to add the `--c++` switch. When you run C++
like this you must remember that the Felix configuration
data base will not allow automatic linkage, as it does for
Felix programs, unless you modify the source.


Upgrading C++ for autolink
==========================

Felix can autolink C++ as well as Felix, using the Felix
configuration database. 

To enable autolink for C++, all you need to do is
put the requirements in the C++ somewhere, usually
in comments. For example

.. code-block:: cpp

    // @requires package mylib

will tell `flx` that this C++ file requires the package `mylib`.
When linking, `flx` will lookup the configuration database for
the file `mylib.fpc` and link against the binary library as
specified in that package, the same as it would for Felix programs.

This also works if you're building mixed C++ and Felix from
sources. The dependent packages are stored in a file associated
with the C++ source file name in the Felix cache, the same way
as for Felix packages specified by

.. code-block:: felix

    requires package "mylib";

in Felix sources. The upgrade to your C++ code has no impact
on your normal C++ compilation. The library will be linked
against automatically only if `flx` drives the C++ compilation
process.

Note that whilst the package requirements in C++ allow
autolinking, as well as providing search paths for header
files, you have to `#include` the header files in your
C++ in the usual way for C++. `flx` cannot current
inject the header file includes in to C++ you supply
because that would mean the C++ would not be compilable
by a C++ compiler, with any switches.

You do not need to do this if you embed the C++ inside
Felix.



Specifying Header file search paths
===================================

In order to compile C++ code, or to compile Felix code
which embeds C++ which requires header files, you can
specify a search path on the `flx` command line by:

.. code-block:: bash

   flx --static -Imydir myfile.cpp myfelix.flx

The `-I` switch extends the search paths used for C++
compilation for the C++ source file `myfile.cpp` as well
as for compiling the generated Felix C++ code.
In addition it *aslo* adds the directory to the Felix
library search path, so any Felix files in the specified
directory will be found.




