.. _compactlineartypes:

Compact Linear Types
--------------------

A type which is built entirely from the unit type 1,
tuple or array constructors, and sum constructors,
is called a `compact linear type`.

Values of compact linear types have a special representation
as a 64 bit integer.

A type which is a sum of N units is called a unitsum type,
the types as written with a decimal integer for the number
of cases, the representation is as an unsigned integer 
in the standard representation with values from 0 to
N-1.

The size of 0 is 0, of 1 is 1, and of N is N.
The value of a product A0 * A1 is given by (N0 + size(A0) :math:`\times` N1)
and ranges from 0 to size(A0) * size(A1). To otain the first component
find the remainder with respect to size(A0), for the second 
component find the quotient instead.

The value of a sum A0 + A1 is given by N0 + size(A1) + A :math:`\times` N1.
This is somewhat confusing so here is the way to decode such
a value: if the value is less than or equal to size(A0),
the case is the first one (case 0), and has the given value.
Otherwise, subtract size(A1) to obtain the second
componant (case 1).

These encoding and decoding rules can be used recursively
or expanded to an iterative form. The type is compact because
it uses the compact range of values from 0 through to the size
of the type minus 1, it is linear because it is a subrange
of an integer type.

As a consequence the type:

.. code-block:: felix

  2^32

is an array of 32 boolean values and occupires exactly 32 bits.

The primnary purpose of compact linear types if for polyadic
(rank independent) array operations. Operations on higher order
arrays can be reduced to linear operations by coercing a
compact linear index to its linear form.

In Felix compact linear types are addressable! However
the pointers formed are not simple machine addresses.


Pointer syntax
^^^^^^^^^^^^^^

.. code-block:: felix

  satom := "_pclt<" stypeexpr "," stypeexpr ">"
  satom := "_rpclt<" stypeexpr "," stypeexpr ">"
  satom := "_wpclt<" stypeexpr "," stypeexpr ">"

A pointer to a compact linear type `_pclt<D,C>` specifies a pointer to a component 
type `C` embedded in a complete compact linear type `D`, which occupies a machine word.
This type is a subtype of the read-only pointer type `_rpclt<D,C>` and
write only pointer type `_wpclt<D,C>`.

Projections
^^^^^^^^^^^

Projections for components of compact linear products use the same syntax
as for non-compact products, as do the overloads for pointers. In order
to get a compact linear pointer we can first store a complete compact
linear value in a variable. Then starting with an ordinary machine
pointer to the variable, a pointer projection to a component will
yield a compact linear pointer. For example:

.. code-block:: felix

  var x = true,false,true;
  var px = &x;     // ordinary pointer
  var p1 = px . 1; // compact linear pointer
  p1 <- true;      // store 1 bit


