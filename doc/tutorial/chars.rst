Characters
==========

The atoms of a string are ASCII characters.

Type
----

Lifted from C++:

.. code-block:: felix

    type char = "char";

Extraction from String
----------------------

The first character of a string can be extracted:

.. code-block:: felix

    var ch : char = char "Hello";

This sets ch to the letter H. If the string is empty,
the char becomes NUL.

Ordinal Value
-------------

The code point of the character, from 0 to 255, as an `int`:


.. code-block:: felix

    var ch = char "Hello"; // H
    var j = ch.ord; // 72

Construction from Ordinal Value
-------------------------------

Finds the n'th character in the ASCII character set.

.. code-block:: felix

    var ch = char 72; // H






