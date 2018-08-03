Structures
==========

Struct Definitions
------------------

Syntax
^^^^^^

.. code-block:: felix

  stmt := "struct" sdeclname "=" ? "{" sstruct_mem_decl * "}" 
  stmt := "export" "struct" sdeclname "=" ? "{" sstruct_mem_decl * "}" 
    sstruct_mem_decl := sname ":" stypeexpr ";"
    sstruct_mem_decl := stypeexpr sname ";" // C hack
    sstruct_mem_decl := sfunction 

Description
^^^^^^^^^^^

Structs are nominally typed records:

.. code-block:: felix

  struct X {
    a: int;
    b: int;
  }
  var x = X (1,2);

Every struct is equipped with a constructor which is named
after the struct, and which accepts a tuple consisting of
arguments of the types defined in the struct, in order
of writing: these are used to initialise the corresponding
fields.

Structs are products like records, so the field names
can be used as both value projections and pointer projections:

.. code-block:: felix

  var a = x.a; // value
  var pb = &x.b;
 
A special syntax is allowed for structs to make it a bit easier
to import specifications from C:

.. code-block:: felix

  struct X {
    int a;
    &long b;
  }

In this form the type goes first, as in C. However note that the type
specification is a Felix type, so we have to use `&long` for a pointer
to `long` rather than `long*` as in C.

Structs may contain function and procedure definitions:

.. code-block:: felix

  struct X {
    int x;
    fun get => self.x;
    fun getter () self.x;
    proc set (a:int) { self.x <- a; }
  }

Such definitions are *not* methods but ordinary functions
with an extra *mgic* argument so the above is equivalent to:

.. code-block:: felix

  struct X {
    int x;
  }
  fun get (self: X) => self.x;
  fun getter (self: X) () => self.x;
  proc set (self: &X) (a:int) { self.x <- a; }

Note that the argument to a function is a value of the structure
type named `self`. For a procedure, the magic argument is a 
*pointer* instead. The *magic* argument is added automatically
so in the original struct definition the function `get`
appears to have no argument.

Because contained functions and procedure have the hidden
*magic* argument, closures can be formed if there are other
arguments:

.. code-block:: felix

  var a = X(1);
  var asetter = a.set;
  asetter 42;
  var agetter = a.getter;
  println$ agetter();

Note in the example we cannot make a closure for `get`
since it has only one argument.


Cstruct Definitions
-------------------

Syntax
^^^^^^

.. code-block:: felix

  stmt := "cstruct" sdeclname "=" ? "{" sstruct_mem_decl * "}" srequires_clause ";" 

  //$ A hack to help with cut and paste from C headers into Felix
  stmt := "typedef" "struct" "{" sstruct_mem_decl * "}" sdeclname srequires_clause ";"
  ;

  //$ A hack to help with cut and paste from C headers into Felix
  stmt := "typedef" "struct" sdeclname "{" sstruct_mem_decl * "}" sdeclname srequires_clause ";"


A cstruct definition is similar to a struct definition,
except no C++ code is emitted for the type definition.
Instead, a C struct or union of the same name must be
defined, typically in a C include file.

