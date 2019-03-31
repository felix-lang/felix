Filenames
=========

Filename function reside in class Filename.
This class is not open by default, it is recommended the functions
below are called explicitly qualified by `Filename::`.

Path separator
++++++++++++++

A function returning the system path separator, `/` on Unix, and '\\' on Windows:

.. code-block:: felix

  fun sep: 1 -> string;

Path kind
+++++++++

A function detecting if a filename is absolute or relative

.. code-block:: felix

  fun is_absolute_filename : string -> bool;

Root
++++

A function returning the filesystem root directory name, `/` on Unix and 'C:\'
on Windows:

.. code-block:: felix

  virtual fun root_subdir : string -> string;

Binary codefile Extensions
++++++++++++++++++++++++++

Functions returning the extensions (including the dot)

.. code-block:: felix

  virtual fun executable_extension : 1 -> string;
  virtual fun static_object_extension: 1 -> string;
  virtual fun dynamic_object_extension: 1 -> string;
  virtual fun static_library_extension: 1 -> string;
  virtual fun dynamic_library_extension: 1 -> string;


Path Splitting
++++++++++++++

split1 returns a pair consisting of a directory name and basename
with the separator between them lost except in the special case
"/x" where the "/" is kept as the directory name.
If there is no separator, the path is the basename and
the directory name is the empty string (NOT . !!!)

.. code-block:: felix

  fun split1(s:string): string * string;

split a filename into a list of components.

.. code-block:: felix

  fun split(s:string)=> split (s, List::Empty[string]);

Get the basename of a path (last component).

.. code-block:: felix

  fun basename(s:string) :string;

Get the directory name of a path (all but the last component).

.. code-block:: felix

  fun dirname(s:string) : string;
  
Return a list of all the directory names in a path.
For example a/b/c gives "a", "a/b"

.. code-block:: felix

  fun directories (s:string) : list[string];

Split off extension. Includes the dot. 
Invariant: input = basename + extension.
Works backwards until it hits a dot, path separator,
or end of data. If a dot, strip it and the tail of the string,
otherwise return the original string.

.. code-block:: felix

  fun split_extension (s:string): string * string;

Remove an extension from a filename if there is one.

.. code-block:: felix

  fun strip_extension (s:string) => s.split_extension.0;

Get extension if there is one. Includes the dot.

.. code-block:: felix

  fun get_extension (s:string) => s.split_extension.1;


Path joining
++++++++++++

Join two pathnames into a single pathname.
split and join are logical inverses, however join is not
not associative: join("x", join("","y")) = "x/y"
whereas join(join("x",""),"y") = "x//y"
since split pulls components off from the RHS we have to
fold them back from the left

.. code-block:: felix

  fun join(p:string, b:string) : string;

Note it is common to write this:

  fun /(p:string, b:string) => Filename::join (a,b);

in your code.

 
Join all the strings in a list into a pathname.

.. code-block:: felix

  fun join(ps: List::list[string]): string;

