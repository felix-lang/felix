Type Sets
=========

Felix allows you define a set of types:

.. code-block:: felix

   typedef sints = typesetof (short,int,long);
   typedef uints = typesetof (ushort,uint,ulong);

The list of elements of a type set can be either types or
typesets:

.. code-block:: felix

   typedef ints = typesetof (sints,uints, byte);

You can also calculate the union of two typesets with the TeX 
operator `\cup`, the symbol ..math:`\cup`, it also accepts types
as an abuse of notation.

.. code-block:: felix

  typeset ints = sints \cup uints \cup byte;

Membership of a typeset can be tested with the `in` operator:

.. code-block:: felix

   long in ints

The result is a type, either void (0) for false and
unit (1) for true. [This is a design fault, the result
should of kind BOOL]


Typesets are used for constrained polymorphism,
primarily with primitive bindings:

.. code-block:: felix

  fun add[T where T in ints]) : T * T -> T = "$1+$2";
  fun add[T in ints]) : T * T -> T = "$1+$2";
  fun add[T:ints] : T * T -> T = "$1+$2";

The second and third forms are a shortcut versions of the first.

When a query is made of a typeset, it is expanded to a typematch:

.. code-block:: felix

  // T in sints
  typematch T with
  | short => 1
  | int => 1
  | long => 1
  | _ => 0
  endmatch

This means the elements of a typeset can be parameterised by one or more type variables
provided they're in scope. For example:

.. code-block:: felix

  typedef iorspair[S,U] = typesetof (S * S, U * U);
  fun add[T,U,W where T in sints, U in uints, W in iorspair[S,U]) ...

where the last condition expands to

.. code-block:: felix

  typematch W with
  | S * S => 1
  | U * U => 1
  | _ => 0
  endmatch



