Variables
=========

Felix provides  three simple forms to define and initialise variables.
The `var` binder is used to define a variable, it binds a name
to a storage location.

Variables with type and initialiser 
-----------------------------------

A variable can be defined with a type annotation
and an initialiser.

.. code-block:: felix

    var b : bool = true;
    var i : int = 1;
    var s : string = "Hello";
    var d : double = 4.2;
    var f : float = 4.2f;

The specified type must agree with the type of the initialiser.

Variables without type annotation
---------------------------------

A variable can also be defined without a type annotation
provided it has an initialiser.

.. code-block:: felix

    var b = true;
    var i = 1;
    var s = "Hello";
    var d = 4.2;
    var f = 4.2f;

In these cases the type of the variable is the type of the initialiser.

Variables without initialiser
-----------------------------

Variables can be defined without an initialiser.

.. code-block:: felix

    var b : bool;
    var i : int;
    var s : string;
    var d : double;
    var f : float;

In this case the variable will be initialised by the underlying C++
default initialiser. It is an error to specify a variable this way
if the underlying C++ type does not have a default initialiser.

If the underlying C++ default initialiser is trivial, so that the store
is not modified, then the Felix variable is uninitialised.

Simple Assignment
-----------------

An assignment can be used to assign the first value
stored in the location of a variable,
to modify the value which an explicit initialiser previously
provided, or to modify the value which the underlying C++
default initialiser provided.

.. code-block:: felix

    var b : bool;
    var i = 1;
    b = true;
    i = 2;

Assignments are executed when control flows through
the assignment.


Variable Hoisting
-----------------

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

