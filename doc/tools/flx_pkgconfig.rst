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

Path
----

The `--path=` and `--path+=` switches are used to set the search path
used by flx_pkgconfig. The first switch specifies the first directory
on the search path. The second can be used to add directories to the
end of the search path. When looking for a package, the directories
are searched in order.

The `--extension=` switch species the extension of the filename to
use, excluing the `.`. This is `fpc` for Felix databases, or `pc`
for those constructed for pkconfig. Examination of the latter may
or may not succeed.

The `--hide` or `-h` switch specifies package hiding. This means,
once a package is found, its contents are the final, total,
record of fields. By default, flx_pkgconfig finds all occurences
of a package, so that a field named `field`, if not found in the
first record of the package, may still be found in the second.

By default, if a `field` is present in both packages, and the query
is for a list of values, the values of the fields of both packages
will be merged. This machinery allows extending a database with
auxilliary information, without modifying it.

Recursion
---------

By default, flx_pkgconfig only searches the specified list
of packages. If the `-rec` or `-r` switch is given, however,
the search extends to dependent packages, based on the
field named `Requires`. This is the only field with a special
meaning.

If the switch `-rec=` is used, the specified field name is
used for recursion instead of `Requires`.

Action
------

The default action of flx_pkgconfig is simply to verify that a list
of packages given exists. It returns 0 if all the packages exist,
or 1 if one or more is missing.

The `--noerror` switch suppresses error checking.

The `--list` option causes flx_pkgconfig to list, on
a single line, all the packages it finds. Used with
the `-r` switch, this will provide the transitive closure
over dependencies specified by the `Requires` field.

The `--field=` switch specifies to output the contents of the
specified field, instead of the package name. Nothing is output
if the field is missing from a package.

Order of output
---------------

If the `--keepleftmost` switch is specified, then the leftmost
occurence of a value in the output is retained, and duplicates
to its right are dropped. 

If the `--keeprightmost` switch is specified, then the rightmost
occurence of a value in the output is retained, and duplicates
to its left are dropped.

If the `--keepall` switch is specified, then duplicates
are kept.

If the `-b` or `--backwards` switch is specified, then the final
list of values to be output is reversed just before printing it.

flx_pkgconfig accumulates field values as it sees them. It processes
a file from top up to any `Requires` field. If recursion is enabled,
then it will process any packages specified in the `Requires` field
next, before continuing to process the current file. This means
field specified before a `Requires` field are gathered before
dependencies, and fields specified after a `Requires` field
follow those dependencies.

Unix linkers normally requires that a library A requiring a library
B be specified first. This means that when processing object files
or libraries, the linker gather external references as it progresses
from left to right in the link order, and satifies them as it finds
external definitions. Thus, a definition seen prior to a reference
will not satisfy it.

Other linkers require that external definitions be given first, so that
when a reference is seen it is immediately satisfied. If the definition 
comes later, it will not resolve the reference.

Some linkers allow gathering of all the definitions and references in
any order and then satify the references from the currently gathered set.

Because the ordering can matter, you must order fields in a flx_pkgconfig
data base carefully, and use the order control switches.

Batched requests
----------------

Because searches can be long, you can put any sequence of switches
and package names that would appear in a particular order into
a control file. In this case, newlines may also separate the
options. The sequence can then be includes by specifying the
control filename prefixed by `@`. Control files can also 
refer to control files. Take care that inclusion is acyclic.
Unlike `Require` fields, flx_pkgconfig follows inclusions
without checking for cycles.

Merging
-------

By default, if `-keepall` is not specified, duplicate field values
are merged so a value will only occur once in the output.

Field specification
-------------------

A field specification consists of a field name, followed
by a colon `:` and then a list of values. If the list
of values is too long for your taste, the list can be split
over any number of field specifications, that is, the
field name can be repeated.

Values are usually separated by spaces.
If the value need to contain spaces, it can be quoted
with single quote makes `'` or double quote marks `"`.
Quoting is also necessary if the value must contain quote marks.
This is often the case for include file names, so you will see:

.. code-block::

   includes: '"myheader.h"' <sys.h>

for example. Without the outer single quotes the first entry would lose
the inner quotes which are part of a C include file specification.

Comments and ignored lines
--------------------------

Fpc files can contain blank linkes anywhere.
You can also put lines starting with `#` as comments.

Substitutions
-------------

Flx_pkgconfig also allows substitutions. A line consisting
of an identifier, followed by an `=` sign, followed by a value,
defines a macro.

When processing field values, flx_pkgconfig replaces `${macroname}`
with the specified value of a macro, if it exists.

Special Field handling
----------------------

On some systems, options are given by two successive arguments.
For example on OSX, frameworks are specified by

.. code-block:: bash

    -framework OpenGL

Unfortunately, flx_pkgconfig cannot be given two values, `--framework` and
`OpenGL` because the result would usually be a single `--framework` value
and a lot of frameworks. On the other hand, you cannot specify a single
value with quotation, because although this will collate correctly,
the output to the `flx` tool, unlike the command line output, will
be a single word which is passed to the shell, causing the compiler
to get one argument, when it needs two in order.

The convention is to code this as:

.. code-block:: bash

    `---framework=OpenGL`

that is, a single value, with a triple leading `-` character, and spaces
replaced by `=` character. This is the format sane systems would use.
Then, the two leading dashes are stripped off, and the remainder
split by the equals character. `flx` uses this convention when
processing results returned by `flx_pkgconfig` internally under
program control. This is not part of the `flx_pkgconfig` system
or tool, but a way to post-process the results if necessary.




