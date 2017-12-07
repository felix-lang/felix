Comments
========

Felix has two kinds of comments.

C++ comments
------------

C++ style comments begin with `//` and end at the end of
the line.

.. code-block:: felix

    println$ "Hi"; // say Hi!

Nested C comments
-----------------

C style comments begin with `/*` and end with a matching `*/`.

.. code-block:: felix

    /* This is an introductory program,
       which says Hello!
    */
    println$ "Hello"; /* Say it! */
   


Unlike C comments, in Felix C style comments nest.
Take care of and leadin or leadout marks hidden in string
literals!

.. code-block:: felix

    // a comment in C++ style
    /* a C style comment
       /* nested comment */
       still commented
    */

Nested comments are often used to temporarily remove
code:

.. code-block:: felix

    /*
    /* This is an introductory program,
       which says Hello!
    */
    println$ "Hello"; /* Say it! */
    */
    println$ "Bye";


   


