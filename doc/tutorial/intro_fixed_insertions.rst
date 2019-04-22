Fixed Insertions
================

Felix also allows you to insert executable code literally in fixed places
inside Felix code. There are two cases.

Lifting Plain Statements
------------------------

Statements can be lifted with the plain `cstmt` statement:

.. code-block:: felix

  proc hello() {
    cstmt """
      std::cout<< "Hello World" << ::std::endl;
      std::cout<< "C++ Embedded in Felix" << ::std::endl;
    """;
  }

Lifting NonReturning Executable Statements
------------------------------------------

If the C statement does not return, use the `noreturn` option:

.. code-block:: felix

  proc leave() {
    noreturn cstmt "::std::exit(0);";
  }


Lifts with arguments
--------------------

Lifted code can accept arguments:

.. code-block:: felix

  proc error(x:int) {
    noreturn cstmt "::std::exit($1);" x;
  }

The argument can be a tuple, the components are inserted
by the usual rules for C bindings using `$9` style notation.


Lifting Expressions
-------------------

An expression can be lifted too, however the type
must be given:

  
.. code-block:: felix

  var x = cexpr[int]"42" endcexpr;

If the expression is not atomic, it is wise to enclose it
in parentheses.

Lifting Variables
-----------------

There is a short hand for lifting variables:

.. code-block:: felix

  cvar[int] M_PI;

which is equivalent to:

.. code-block:: felix

  cexpr[int]"M_PI" endcexpr;



