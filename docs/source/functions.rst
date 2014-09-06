
Functions
=========

`Syntax <http://felix-lang.org/share/lib/grammar/functions.flxh>`_

A felix function definition takes one of three basic forms:

.. code-block:: felix
   
   fun f (x:int) = { var y = x + x; return y + 1; }
   fun g (x:int) => x + x + 1;
   fun h : int -> int = | ?x => x + x + 1;

The first form is the most general, the body 
of the function contains executable statements
and the result is returned by a return statement.

The second form is equivalent to a function in the first
form whose body returns the RHS expression.

The third form specifies the function type then the
body of a pattern match. It is equivalent to

.. code-block:: felix
   
   fun h (a:int) = { return match a with | ?x => x + x + 1 endmatch; }

The first two forms also allow the return type to be
specified:

.. code-block:: felix
   
   fun f (x:int) : int = { var y = x + x; return y + 1; }
   fun g (x:int) :int => x + x + 1;

Functions may not have side effects.

All these function have a type:

.. code-block:: felix
   
   D -> C

where D is the domain and C is the codomain: both would
be ``int``_ in the examples.

A function can be applied by the normal forward
notation using juxtaposition or what is whimsically
known as operator whitespace, or in reverse notation
using operator dot:

.. code-block:: felix

   f x
   x.f

Such applications are equivalent.  Both operators are left
associative. Operator dot binds more
tightly than whitespace so that

.. code-block:: felix
   
   f x.g    // means
   f (g x)

A special notation is used for application to the unit tuple:

.. code-block:: felix
   
   #zero // means
   zero ()

The intention is intended to suggest a constant since a pure
function with unit argument must always return the
same value. 

This hash operator binds more tightly than operator dot so

.. code-block:: felix
   
   #a.b // means
   (#a).b


Pre- and post-conditions
------------------------

A function using one of the first two forms
may have pre-conditions, post-conditions, or both:

.. code-block:: felix
   
   fun f1 (x:int when x > 0) => x + x + 1;
   fun f2 (x:int) expect result > 1 => x + x + 1;
   fun f3 (x:int when x > 0) expect result > 1 => x + x + 1;

Pre- and pos-conditions are usually treated as boolean assertions
which are checked at run time. The compiler may occasionally be able
to prove a pre- or post-condition must hold and elide it.

The special identifier ``result``_ is used to indicate the return
value of the function.

Higher order functions
----------------------

A function may be written like

.. code-block:: felix
   
   fun hof (x:int) (y:int) : int = { return x + y; }
   fun hof (x:int) (y:int) => x + y;

These are called higher order functions of arity 2.
They have the type

.. code-block:: felix
   
   int -> int -> int   // or equivalently
   int -> (int -> int) //since -> is right associative.

They are equivalent to

.. code-block:: felix
   
   fun hof (x:int) : int -> int = 
   {
     fun inner (y:int) : int => x + y;
     return inner;
  }

that is, a function which returns another function.

Such a function can be applied like

.. code-block:: felix
   
   hof 1 2 // or equivalently
   (hof 1) 2

since whitespace application is left associative.

Procedures
----------

A function which returns control but no value is called a procedure.
Procedures may have side effects.

.. code-block:: felix
   
   fun show (x:int) : 0 = { println x; }
   proc show (x:int) { println x; }
   proc show (x:int) => println x;

The second form is a more convenient notation.
The type 0 is also called ``void``_ and denotes
a type with no values.

A procedure may return with a simple return statement:

.. code-block:: felix
   
   proc show (x:int) { println x; return; }

however one is assumed at the end of the procedure
body .

Procedures can also have pre- and post-conditions.

A procedure may be called like an application,
however it must be a whole statement since
expressions of type void may not occur interior
to an expression.

.. code-block:: felix
   
   show 1;
   1.show;

If a procedure accepts the unit argument, it may be elided:

.. code-block:: felix
   
   proc f () =>  show 1;
   f; // equivalent to
   f ();

Generators
----------

TBD


