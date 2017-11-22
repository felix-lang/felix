===============
Basic Operation
===============

Felix is designed to be used, in the first instance,
like Python or other scripting language. You can just
run a text file directly like this:

.. code-block:: bash

    flx hello.flx


Behind the scenes, the `flx` command will first run the
Felix compiler executable `flxg` generating some C++ 
files and some other information.

Then, it runs your chosen C++ compiler to compile and link those
files to binary form.

Finally, it runs the binaries.

You do not need make, autoconf, or any kind of build script
or configuration, it just works!

Where is the executable?
========================

Felix puts the executable here:

.. code-block:: text

   $HOME/.felix/cache/binary/$HOME/felix/hello.so

where `$HOME` is your home directory. In other words,
Felix uses a cache, which by default is

.. code-block:: text

    $HOME/.felix/cache

to store temporary files. Text files go in:

.. code-block:: text

    $HOME/.felix/cache/text

and binary files go in:

.. code-block:: text

    $HOME/.felix/cache/binary


The file name used in the appropriate cache uses the prefix of the *absolute*
pathname of the source file.

That's a library not an executable!
===================================

You're right! By default, Felix builds libraries, not programs,
and it builds your program as a shared library. On Linux, this
will have the extension `.so`, on OSX it will be `.dylib` and
on Windows it will be `.dll`.

These library objects are loaded at run time by a small
program executable, it will be called either `flx_run`
or `flx_arun`, which can be found in `build/release/host/bin`.
It will have extension `.exe` on Windows and no extension on
unix like systems. This program is passed the absolute pathname
of the library to load, which it does using `dlopen()` on unix
like systems and `LoadLibrary()` on Windows.

How can I avoid rebuilding my program every time?
=================================================

That's easy. Make a cup of coffee. There's nothing
to do. Felix automatically checks dependencies and
if it would compile the same program you have already
compiled it just runs the already compiled one.

How can I make a standalone executable?
=======================================

You can best do this like so:

.. code-block:: bash

    flx --static -c -od . hello.flx

First, the `--static` switch says to do static linkage,
instead of dynamic linkage. Second, the `-c` switch says
to compile and link the program but not run it.
Third, the `-od .` switch says to put the output of the
build process into the current directory `.` using the
basename of the source file `hello` as the basename
of the executable.

On Unix systems `hello` will appear in the current directory
and you can run it by 

.. code-block:: bash

    ./hello


On Windows, `hello.exe` will appear instead,
and you can run it by:

.. code-block:: bash

    hello

You can copy this program wherever you like and it will work,
it does not need Felix anymore.

Note that for some complex programs which uses plugins,
the program will have to be able to find the plugins,
which are shared libraries or DLLs, and you will need to also
set up an environment where it can do so. The `flx` program,
even though a statically linked executable, can still load
plugins.

























