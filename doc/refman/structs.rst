Structures
==========

Struct Definitions
------------------

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
  



Cstruct Definitions
-------------------

A cstruct definition is similar to a struct definition,
except no C++ code is emitted for the type definition.
Instead, a C struct or union of the same name must be
defined, typically in a C include file.

