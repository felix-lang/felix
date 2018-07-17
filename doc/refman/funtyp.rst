Function Types
==============

Felix has 4 primary classes of executable components.

The general type of a function, generator, or procedure
is given by 

.. code-block:: felix

  D -> [E] C

where D is the domain, C is the codomain, and E is a type representing
effects. Generally the effects value is not written and defaults
to unit. If C is void (0), the type denotes a procedure, otherwise
it is a function or generator.

Values of these types are pointers to a procedure or function
object.

The type:

.. code-block:: felix

  D --> C

is the type of a C/C++ function pointer. It can be used where a 
Felix function is required. If C is void, the C function is
returning C void. If D is unit or 1, the C function has no
arguments. Multiple function parameters are encoded with
a tuple type.

Do not confuse C functions with function primitive bindings,
closures or which are Felix function type.



