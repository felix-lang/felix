Procedures
==========

A sequence of statements can be wrapped into a named
entity called a `procedure`. In addition, a procedure
may accept an argument. The accepting variable is
called a parameter.

Type
----

The type of a procedure with parameter type T is written

.. code-block:: felix

    T -> 0

A procedure is a subroutine, it returns control, but
it does not return a value. To be useful, a procedure
must change the state of the program or its environment.
This is called an effect.

Procedures in Felix are first class and can be used as values.

Definition
----------

A procedure is defined like this:

.. code-block:: felix

    proc doit (x:int) {
       println$ x;
       x = x + 1;
       println$ x;
    }

A procedure may explicitly return control when it is
finished.

.. code-block:: felix

    proc maybedoit (x:int) {
      if x > 0 do
        println$ x;
        return;
      done
      x = -x;
      println$ x;
    }


If the procedure does not have a return statement at the end,
one is implicitly inserted.

A procedure can have a unit argument:

.. code-block:: felix

    proc hello () { 
      println$ "Hello";
    }


Invocation
----------

A procedure is called with a call statement. The identifier `call`
may be omitted. If the argument is unit, it also may be omitted.

.. code-block:: felix

    proc hello () { 
      println$ "Hello";
    }
    call hello ();
    hello ();
    hello;












