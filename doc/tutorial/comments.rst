Comments
========

Felix has two kinds of comments.

C++ comments
------------

C++ style comments begin with `//` and end at the end of
the line.

Nested C comments
-----------------

C style comments begin with `/*` and end with a matching `*/`.
Unlike C comments, in Felix C style comments nest.
Take care of and leadin or leadout marks hidden in string
literals!

.. code-block:: felix

    // a comment in C++ style
    /* a C style comment
       /* nested comment */
       still commented
    */


