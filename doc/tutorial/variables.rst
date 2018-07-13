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

    var c : bool = true;
    var k : int = 1;
    var s : string = "Hello";
    var b : double = 4.2;

The specified type must agree with the type of the initialiser.

Variables without type annotation
---------------------------------

A variable can also be defined without a type annotation
provided it has an initialiser.

.. code-block:: felix

    var c = true;
    var k = 1;
    var s = "Hello";
    var b = 4.2;

In these cases the type of the variable is the type of the initialiser.

Variables without initialiser
-----------------------------

Variables can be defined without an initialiser.

.. code-block:: felix

    var c : bool;
    var k : int;
    var s : string;
    var b : double;

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

Note that in Felix, the scope of a declaration or definition
of a name is the whole of the containing scope, similar to 
the scope of a label in a function in C, or a member function in 
a C++ class. 

This results in  what is called "set-wise lookup" since all lookups 
in a scope can find all identifiers declared in that scope (not just
previously declared ones).
    
