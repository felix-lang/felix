Variables
=========

Felix provides  threer simple forms to define and initialise variables.
The `var` binder is used to define a variable, it binds a name
to a storage location.

Variables with type and initialiser 
-----------------------------------

A variable can be defined with a type annotation
and an initialiser.

.. code-block:: felix

    var c : bool = true;
    var k : int = 1;
    var s : string = "Hello";
    var b : double = 4.2;


Variables without type annotation
---------------------------------

A variable can also be defined without a type annotation
provided it has an initialiser.

.. code-block:: felix

    var c = true;
    var k = 1;
    var s = "Hello";
    var b = 4.2;

Variables without initialiser
-----------------------------

Variables can be defined without an initialiser.

.. code-block:: felix

    var c : bool;
    var k : int;
    var s : string;
    var b : double;


Simple Assignment
-----------------

An assignment can be used to assign the first value
stored in the location a variable initialises,
or, to modify the value which is stored later.

.. code-block:: felix

    var c : bool;
    var k = 1; 
    c = false; 
    k = 2;

Assignments are executed when control flows through
the assignment.


Semantics of var binder
-----------------------

Var binders are equivalent to declaration of an uninitialised
variable and an assignment. The location of the declaration
within the current scope is not relevant. The position of
an initialising assignment is. For example:

.. code-block:: felix

    a = 1;
    var b = a;
    var a : int;

is equivalent to

.. code-block:: felix

    var a = 1;
    var b = a;

In Javascript this is called hoisting. The var binder with
initialiser is syntactic sugar for two statements, an uninitialised
variable declaration, and a separate assignment.


    
