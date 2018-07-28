Subtyping
=========

Felix supports a specific set of subtyping coercions. Subtyping coercions
can be applied manually, and, in specific circumstances automatically.

Passing an argument to a function or procedure is subject to automatic
subtyping. Assigning a variable or initialising a value inline does *not*
admit automatic subtyping.

Tuples
------

Tuples and arrays support depth subtyping but not width subtyping.
Width subtyping would allow you to pass a tuple with an overlong
tail which would be silently chopped off.

Records
-------

Records subtyping is covariant and supports both width and depth subtyping.
A record argument may have more fields than the parameter is matches.
The field component values may also by subtypes of the 
corresponding parameter fields.

Polymorphic Variants
---------------------

Polymorphic variant subtyping is covariant and supports both with
and depth subtyping. A variant argument may have less fields than
the parameter it matches. Constructor argument values may also be
subtypes of the corresponding parameter constructors.

Pointers
--------

Read only and write only pointers are both subtypes of read-write
pointers. The pointed at types must be equal.

Functions
---------

Functions are covariant in their domain and contravariant
in their codomain.

Nominal Types
-------------

Nominal types including primitives, structs and unions do not
admit subtying relations by default. However the programmer
can write a single step coercion. Currently coercions do not chain.
The coercions are done before overloading and so can result
in an ambiguity even of one function parameter matches exactly
and the other requires a coercion.

.. code-block:: felix

  supertype: long (x:int) => x.long;

Polymorphic Specialisation
--------------------------

Polymorphic specialisation is a subtyping relation, but we call
it subsumption. The argument of a function must be more
specialised than the parameter. Unlike other subtyping steps,
overloads select the most function or procedure with the most
specialised matching parameter.

[This rule should be applied for subtyping as well as subsumption
but isn't.]


