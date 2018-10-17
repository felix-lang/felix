Lists
=====

Lists are a fundamental, polymophic data type. Felix list is a singly linked,
purely function data type. All the values in a list have the same type.

Creating a list.
---------------

A list can be created from an array:

.. code-block:: felix

  var x = list (1,2,3,4);

A more compact notation is provided as well:

.. code-block:: felix

  var x = ([1,2,3,4]);

Empty lists
-----------

For an empty list there are two notations:

.. code-block:: felix

  var x = list[int] (); // empty list of int
  var y = Empty[int];   // empty list of int



Displaying a list
-----------------

A list can be converted to a human readable form with the `str` function,
provided the values in the list can be converted as well:

.. code-block:: felix

  println$ "A list is " + ([1,2,3,4)].str;

Concatenation
-------------

Lists can be concatenated with the `+` operator:

.. code-block:: felix

  println$ list (1,2,3) + ([4,5,6]);

Length
------

The length of a list is found with the `len` function, the result
is type `size`:

.. code-block:: felix

  println$ ([1,2,3,4]).len; // 4

Prepending an element
---------------------

A new element can be pushed on the front of a list with the Cons function
or using the infix `!` operator, or even with `+`:

.. code-block:: felix

  var a = ([2,3,4]);
  var b = Cons (1, x);
  var c = 1 ! a;
  var d = 1 + a;

The lists `b`, `c` and `d` all share the same tail, the list `a`.
This means the prepend operation is O(1). It is safe because 
lists are immutable.

The use of `+` is not recommended because it is rather too heavily overloaded.
In particular note:

.. code-block:: felix

   1 + 2 + ([3,4]) // ([3,3,4])
   1 + (2 + ([3,4]) // ([1,2,3,4])

because addition is left associative.

Pattern matching lists
----------------------

Lists are typically decoded by a recursive function that does
pattern matching:

.. code-block:: felix

  proc show(x:list[int]) =>
    match x with
    | Empty => println$ "end"
    | head ! tail => 
      println$ "elt= " + head.str;
      show tail;
    endmatch
  ;

The text between the `|` and `=>` is called a pattern. To analyse a list,
there are two cases: the list is empty, or, the list has a head element
and a following tail. The procedure prints "end" if the list is empty,
or the head element followed by the tail otherwise.



