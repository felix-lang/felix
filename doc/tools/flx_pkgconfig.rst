====================================================
The Package Configuration Manager Tool flx_pkgconfig
====================================================

Felix uses a configuration database to locate resources
required to build and run programs. The code to inspect
this database is built in to the `flx` tool, however
there is a standalone executable, `flx_pkgconfig` you can
use from the command line.

flx_pkgconfig
=============

To run the `flx_pkgconfig` tool, it needs to be on your `$PATH`.
This should be the case if you have installed Felix, or, if you have
set Felix up for in-place operation. You should try this to verify
if the system can locate it:

.. code-block:: bash

   flx_pkgconfig --help

which should output something like this:

.. code-block:: text

    flx_pkgconfig [options] pkg pkg ...
      returns code 1 if any packages are missing unless --noerror is specified
      prints package or field list to standard output on one line
    options: (follows GNU conventions)
      --path=dirname        set database directory name
      --path+=dirname       append database directory name
      --extension=fpc       set resource descriptor extensions,
                              default 'fpc' use 'pc' for pkgconfig databases
      -h
      --hide                only process first package in path with a given name
                              default, process all occurences
      --list                list available packages from specified set
      --missing             list missing packages from specified set
      --noerror             do not return 1 because of missing packages
      -r
      --rec                 form transitive closure of specified set based on Requires field
      --rec=field           form transitive closure of specified set based on specified field
      -b
      --backwards           process specified packages in reverse order
      --field=field         collate values of field in package set
      --keepleftmost        remove duplicate values in output keeping only leftmost occurrence
      --keeprightmost       remove duplicate values in output keeping only rightmost occurrence
      --keepall             keep duplicate values in output
      @filename             Replace with arguments from filename, one line per argument

flx_pkgconfig queries configuration databases for information about
packages. From the repository root, try this:

.. code-block:: bash

    ~/felix>flx_pkgconfig --path=build/release/host/config --field=Requires flx_run
    flx_pthread flx flx_gc flx_dynlink flx_strutil


This causes flx_pkgconfig to search the configuration database in the directory
build/release/host/config, and find all the packages on which the package `flx_run`
depends.

It does this by seeking the field `Requires` in `flx_run.fpc`, and then recursively
examining all the packages required, as specified by that field.

Now try this:

.. code-block:: bash

    ~/felix>flx_pkgconfig --path=build/release/host/config --field=provides_slib -r flx_gc
    -lflx_gc_static -ljudy_static -lflx_exceptions_static

Here, we want to find the contents of the field `provides_slib`. We have to use
the `-r` switch to also make flx_pkgconfig recursively follow all the `Requires`
fields. The result is the list of all the values of any `provides_slib`
fields found in the transivitive closure of the recursive search.

You would use this command to generate the flags needed to pass to your linker,
in order to link library `flx_gc` statically. On the other hand to link
dynamically you would use this instead:

.. code-block:: bash

    ~/felix>flx_pkgconfig --path=build/release/host/config --field=provides_dlib flx_gc
    -lflx_gc_dynamic

The reason is that shared libraries link their own dependencies.
It is important *not* to specify the transitive closure, because that
would pre-empt the linker, and might pick the wrong version. 

flx_pkgconfig is *not* a special purpose program, unlike its precursor,
pkgconfig. It is, in fact, a fully general, if simplistic, database
query tool, for databases consisting of records represented by files
with lines of fields, each field having a name and one or more values.

Here is a sample `.fpc` file:

.. code-block:: text

    ~/felix>cat build/release/host/config/flx_gc.fpc
    Generated_from: 2403 "/Users/skaller/felix/src/packages/gc.fdoc"
    Name: flx_gc
    Platform: Unix
    Description: Felix default garbage collector (Unix)
    provides_dlib: -lflx_gc_dynamic
    provides_slib: -lflx_gc_static
    includes: '"flx_gc.hpp"'
    library: flx_gc
    macros: BUILD_FLX_GC
    Requires: judy flx_exceptions
    srcdir: src/gc
    src: .*\.cpp


This file contains more than information required to use the
Felix garbage collector. It also contains enough information for
the Felix build system to build it. Normally with third party
libraries, you build it with the vendors build instructions,
but for Felix own components, those build instructions are put
in the `fpc` file to localise the information about the library
in one place.


