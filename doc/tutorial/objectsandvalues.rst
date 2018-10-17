Objects, Values and Pointers
============================

Felix supports both functional and imperative programming styles.
The key bridge between these models is the pointer. 

An array in Felix is an immutable value, it cannot be modified
as a value. However an array can be stored in a variable, which is
the name of an object. An object has two significant meanings:
it names a storage location, and also refers to the value located
in that store.

In Felix, the name of a variable denotes the stored value,
whilst the so-called address-of operator applied to the variable
name denotes the location:

.. code-block:: felix

  var x = 1,2,3,4;  //1: x is an array value, and ..
  var px = &x;      //2: it also denotes addressable store
  var y = *px;      //3: y is a copy of x
  px <- 5,6,7,8;    //4: x now stores a new array

This code illustrates how to get the addres of a variable
on line 2, to fetch the value at the address in line 3,
and to moodify the value at the address, in line 4.

The prefix symbol `&` is sometimes called the address-of operator,
however it is not an operator! Rather, it is just a way to specify
that we want the address of the store a variable denotes, rather
than the value stored there, which is denoted by the plain variable name.

The address is a Felix data type called a pointer type. If a variable
stored a value of type T, the pointer is denoted by type &T.

In line 3 we use the so-called dereference operator, prefix `*`,
to denote the value held in the store to which a pointer points.
Dereference is a real operator.

In line 4, we use the infix left arrow operators, which is
called `store-at`, it is used to store right hand argument
value in the location denoted by the left hand pointer value.

The new operator
================

Felix also provide the prefix `new` operator which copies
a value onto the heap and returns pointer to it.

.. code-block:: felix

  var px = new 42;
  var x = *px;  // x is 42
  px <- 43;     // px now points to 42

This is another way to get a pointer to an object, which allows
the value stored to be replaced or modified.

Pointer projections
===================

All product types including arrays, tuples, records, and structs
provide value projections for fetching parts of the value,
the parts are called components:

.. code-block:: felix

  var ax = 1,2,3,4;                  // array
  var ax1 = x.1;                     // apply projection 1 to get value 2

  var tx = 1, "hello", 42.0;         // tuple
  var tx1 = tx.1;                    // apply projection 1 to get value "hello"

  var rx = (a=1, b="hello", c=42.0); // record
  var rx1 = rx.b;                    // apply projection b to get value "hello"

  struct X {
    a:int;
    b:string;
    c:double;
  }
  var sx = X (1, "hello", 42.0);      // struct
  var sx1 = sx.b;                     // apply projection b to get value "hello"


Arrays and tuples have numbered components, and thus are accessed by
numbered projections, records and structs have named components and thus
values are accessed by named projections.

Although the indicators here are numbers and names, value projections
are first class functions. The functions and their types, respectively, are:

.. code-block:: felix

  proj 1: int^4 -> int
  proj 1: int * string * double -> string
  b: (a:int, b:string, c:double) -> string
  b: X -> string

These are value projections. To store a value in a component of
a product type, we must first obtain a pointer to the store
in which it is located, and then we can apply a *pointer projection*
to it, to obtain a pointer to the component's store. Then we can
use the store-at procedure to set just that component, leaving
the rest of the product alone:

.. code-block:: felix

  &ax . 1 <- 42;         // array
  &tx . 1 <- "world";    // tuple
  &tx . b <- "world";    // record
  &sx . b <- "world";    // struct

In each case we use the same projection index, a number or a name,
as for a value projection, but the projections are overloaded
so they work on pointers too. These pointer projections are
first class functions, here are their types, respectively:

.. code-block:: felix

  proj 1: &(int^4) -> &int
  proj 1: &(int * string * double) -> &string
  b: &(a:int, b:string, c:double) -> &string
  b: &X -> &string

What is critical to observe is that pointers are values,
and the pointer projections are first class, purely functional
functions. Unlike C and C++ there is no concept of lvalues
or references. The store-at operator is a procedure,
and so it is used in imperative code, but the calculations
to decide where to store are purely functional.

The programmer should note that C address arithmetic is
also purely functional, however, C does not have any well
typed way to calculate components of products other than
arrays: you do the calculations only by using the `offsetof`
macro and casts.

C++ has pointers to members, but the calculus is incomplete,
they cannot be added together!

In Felix, projections are functions so adding component offsets
in products is, trivially, just function composition!







