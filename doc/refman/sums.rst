Sum Types
=========

Sum types are a positionally accessed structural type dual to tuples.
They're used like this:

.. code-block:: felix

  typedef integ = short + int + long;
  fun show (x:integ) => 
    match x with
    | `0 s => "short="+ s.str
    | `1 i => "int="+ i.str
    | `2 l => "long="+ l.str
    endmatch
  ;
  var x = `1:integ 42;
  println$ "Value " + show x;

With sums, the cases are numbered from 0 up.
To specify a value, the constructor index, sum type,
and any argument must be given.


Unitsums
--------

A special case of sum types is a sum of units.
Recall the type of an empty tuple is designated as 1 or unit.
Then a unit sum has a type like this:

.. code-block:: felix

   typedef three = unit + unit + int;
   typedef three = 1 + 1 + 1;
   typedef three = 3;

All these forms are equivalent. An integer given as a type
is takens to be a sum of that many units.

Apart from 1, which is a unit sum of 1 unit, there are two
other important unit sums named in the library:

.. code-block:: felix

   typedef void = 0;
   typedef bool = 2;

The type void is a type with no values.
The type bool is a type with two values.
The names false and true are synonyms for \`0:2 and \`1:2.

Unit sums are important because they're used as array indices.
For example an array of 4 ints has the type

.. code-block:: felix

  int ^ 4

The 4 there is not an integer, but the type 4.

Sum types are not used very often because remembering cases
by number is hard.

