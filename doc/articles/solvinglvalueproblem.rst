Solving the Lvalue Problem
==========================

In computing, there is a notion of a thing called an object which is
has two properties: it is a region of store containing a value,
and, as storage, it has an address.

Objects are fundamental to computer science, yet the method of dealing
with them in programming languages is badly broken .. in all languages.

Objects in C
------------

To understand the issue we must first go back to C. In C, a variable is
an object. If the variable is used in an expression, it means the value
stored in the object. If it is used on the LHS of an assignment,
or as the argument of the address-of operator, it refers to the store
itself, that is, it means the address.

To handle this situation, in C, we specify two contexts. The argument
of the address-of operator, and some other operators such as prefix ++,
and, the LHS of an assignment, is called a left-, or l-context,
other subexpressions are called right or r-contexts.

Now, we specify that most expressions are right, or r-values, but
some, such as a variable name or pointer dereference, as said
to be left- or l-values.

Context and value kinds are identified entirely syntactically.
C then has a rule, which says that only an l-value may be used
in an l-context. Both l- and r-values can be used in an r-context.
If an l-value is used in an r-context, its meaning is degraded
to that of an r-value. 

The idea is that an r-values refers to a value whereas an
l-value refers to the store of an object, that is, an l-expression
specifies an address.

The machinery is a weak attempt to conform to socially accepted
norms of mathematical notation in an inappropriate context:
in mathematics an equation is not an operation but a specification,
in computing an assignment is an operation. The rule catches
possible errors, where the programmer tries to assign to a value
instead of an object: such assignments are harmless but rarely intended.

I will note in passing a complication was introduced in C which
invalidates the original K&R concept, namely const. Assignent
to a const lvalue isn't allowed, and it isn't possible to tell
if an lvalue is const syntactically. We need type information
as well.

Objects in C++
--------------

In C++, much more powerful notions of objects are introduced.
A big mistake was made, attempting to preserve the idea of lvalues
by introducing references.

Whereas the rules in C do work, although they're unnecessary semantically,
in C++ the rules do not work. The initial problem in C++ is that with
operator overloading it is no longer possible to tell syntactically if

.. code-block:: c++

  *p

is an lvalue or not, because the dereference operator can be overloaded.

Now remember ISO C already broke the rules of K&R C by introducing const.
So since we already need type information to determine the correctness
of an assignment, C++ introduced references. With references, we no longer
depend on syntax even to determine if an expression is an lvalue,
we need type information. We need that for const anyhow, so it seems there
is no loss getting the lvalueness property from the type system as well.

Unfortunately, considering references as types is unsound. More precisely,
the referencing operation is a type constructor, but it fails to be
combinatorial. 

This actually doesn't matter if the type system is monomorphic,
however with polymjorphism, all hell breaks loose if constructors
are not combinatorial. Pointer formation is combinatorial,
reference formation is not.




