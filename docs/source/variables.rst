Variable Definitions
====================

`Syntax <http://felix-lang.org/share/lib/grammar/variables.flxh>`_

A definition is a statement which defines a name, but does
no cause any observable behavior, or, a class statement, or, 
a var or val statement. The latter two exceptions define a name
but may also have associated behaviour.

The ``var``_ statement
---------------------

The ``var``_ statement is used to introduce a variable name
and potential executable behaviour. It has one of three 
basic forms:

.. code:: felix
   
   var x : int = 1;
   var y : int;
   var z = 1;

The first form specifies the type and an initialising
expression which must be of the specified type.

The second form specifies a variable of the given type
without an explicit initialiser, however the variable
will be initialised anyhow with the default contructor
for the underlying C++ type, although that constructor
may be trivial.

The third form does not specify the type, it will be deduced
from the initialiser.

If the initialiser has observable behaviour it will be observed
if at all, when control passes through the variable statement.

If the variable introduced by the ``var``_ statement is not used,
the variable and its initaliser will be elided and any observable
behaviour will be lost.

To be used means to have its address taken in a used expression,
to occur in a used expression. A used expression is one which
initialises a used variable, or, is an argument to function
or generator in a used expression, or an argument to a procedure
through which control passes. 

In other words, the variable is used if the behaviour of
the program appears to depend on its value or its address.

The library procedure ``C_hack::ignore``_ ensures the compiler
believes a variable is used:

.. code:: felix
   
   var x = expr;
   C_hack::ignore x;

so that any side effects of @{expr} will be seen.
In general the argument to any primitive function, generator
or procedure will be considered used if its containing 
entity is also considered used. In general this means there
is a possible execution path from a root procedure of the
program.

A variable may have its address taken:

.. code:: felix
   
   var x = 1;
   var px = &x;

it may be assigned a new value directly or indirectly:

.. code:: felix

   x = 2;
   px <- 3;
   *px = 4;

A variable is said to name an object, not a value.
This basically means it is associated with the address of a typed
storage location.

Multiple variables
^^^^^^^^^^^^^^^^^^

Multipls variables can be defined at once:

.. code:: felix
   
   var m = 1,2;
   var a,b = 1,2;
   var c,d = m;

With this syntax, no type annotation may be given.

The ``val``_ statement.
----------------------

A ``val``_ statement defines a name for an expression.

.. code:: felix
   
   val x : int = 1;
   val z = 1;

The value associated with a ``val``_ symbol may be computed
at any time between its definition and its use, and may
differ between uses, if the initialising expression depends
on variable state, such as a variable or call to a generator.

It is not an error to create such a dependence since either
the value may, in fact, not change, or the change may
not be significant.

Nevertheless the user must be warned to take care
with the indeterminate evaluation time and use
a ``var``_ when there is any doubt.

Since a ``val``_ simply names an expression, it is associated
with a value not an object and cannot be addressed
or assigned to. However this does NOT mean its value cannot
change:

.. code:: felix
   
   for var i in 0 upto 9 do
     val x = i;
     println$ x;
   done

In this example, x isn't mutable but it does take on
all the values 0 to 9 in succession. This is just a 
most obvious case: a less obvious one:

.. code:: felix
   
   var i = 0;
   val x = i;
   println$ x;
   ++i;
   println$ x;

which is clearly just an expansion of the the first two
iteration of the previously given for loop. However in
this case there is no assurance ``x`` will change after ``i``_
is incremented because the compiler is free to replace
any ``val`` definition with a ``var``_ definition.

Multiple values
^^^^^^^^^^^^^^^

Multiple values can be defined at once:

.. code:: felix
   
   val m = 1,2;
   val a,b = 1,2;
   val c,d = m;

With this syntax, no type annotation may be given.


