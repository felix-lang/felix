C Bindings
==========

In Felix you can use C bindings to lift types from C and C++.
The resulting nominal types are abstract.
Unless otherwise specified, all types lifted from C or C++
must be first class; that is, semi-regular.  This means that values of those types
can be default constructed, copy constructed, copy assigned,
move constructed, move assigned, and destroyed; in other words, behave "like an integer".


To lift immobile objects, lift a pointer instead.

Lifting Primitives
------------------

Here's how you lift primitive types:

.. code-block:: felix

  type int = "int";
  type long = "long";
  type ulong = "unsigned long";
  type address = "void*";

The quoted name must be usable in the forms `T x`, `T *x`. This means you cannot
lift a C function pointer type such as `int (*)(double, long)` because the
variable name has to go just before the `*`. 


Lifting Class types
-------------------

Class types require a bit more work:

.. code-block:: felix

  type vector[T] = "::std::vector<?1>"
    requires header "#include <vector>"
  ;

Here, the `?1` annotation means the first type variable
so that for example `vector[ulong]` lifts `::std::vector<unsigned long>`.

When lifting C++ types make sure to always specify the absolute pathname
of the type. Starting with `::` is strongly recommended. This avoids any
possible ambiguity in generated code.

The `requires` clause with `header` option tells Felix to emit the quoted
text in the header file of generated code, before the type is used.
If the type is not used, the `#include` will not be emitted.

C function types
----------------

C function types can be defined directly in Felix:

.. code-block:: felix

  typedef int2int = int --> int;
  // typedef int (nt2int*)(int)

The type `int2int` is an alias, and the C function type is a structural type,
not nominal type.

Lifting C structs and unions
----------------------------

C structs can be lifted in a way that exposes their fields,
provided the fields are not `const`:

.. code-block:: felix

  header mypair_h = 
  """
    struct mypair { int x; int y; };
  """;

  cstruct mypair {
    x: int;
    y: int;
  } requires mypair_h;

 
Here, to make the code complete, the C definition is given
with floating header code so it can be required by the binding.
Usually, it will be given in a header file.

A `cstruct` works exactly the same as a struct except that
no definition is emitted.

There are caveats! Felix generates the usual elementwise
constructor but it will lead to corruption unless the `cstruct` model
of the type in Felix exactly matches the definition in C.
However this is not required for access to the field components.
In particular the `cstruct` construction in Felix can be used to model
a C union as well.

It's not possible to specify a namespace or class qualification
for the C type.

Lifting functions and procedures
--------------------------------

Since primitive types lifted from C are abstract, we have
to be able to define operations on them with C bindings too.

.. code-block:: felix

  proc push_back[V]: &vector[V] * V = $1->push_back($2);";
  fun front[V]: vector[V] -> V = "$1.front()";
  
  var v : vector[int];
  push_back (&v, 42);
  println$ v.front; // 42


We use `$1` abd `$2` fir the first and second arguments, respectively.

Lifting Constants and Expressions
---------------------------------

You can lift a C constant, variable, or even expressions using
the `const` binder:

.. code-block:: felix

  const pi : double = "M_PI" requires C99_headers::math_h;

Lifting enums
-------------

A special shorthand is available for lifting C enums,
intended for sequences:

.. code-block:: felix

  cenum color = red,blue,green;

This is (roughly) equivalent to:

.. code-block:: felix

  type color = "color";
  const red: color = "red";
  const blue: color = "blue";
  const green: color = "green";
  fun == : color * color -> bool = "$1==$2";

Note in particular equality is automatically defined.
This is required for using the enumeration values in pattern matches.

Lifting Flags
-------------

A special shorthand is available for lifting C enums,
intended for flags:

.. code-block:: felix

  cflags color = red,blue,green;

This defines equality as for cenums, but also makes all
the standard bitwise operations available.





