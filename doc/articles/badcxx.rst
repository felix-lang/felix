What's Wrong With C++
=====================

This article is intended to clearly specify *fundamental* problems in C++.
There are many problems, in any language, but the concern here is with
serious core issues.

Syntax
------

C and C++ have really bad syntax. C started out weak, got worse,
C++ inherited the problems, and then made them worse again.

To understand how bad it is, we look at a brief history.
Originally K&R C was designed so that top level constructions, at least after
pre-processing, could be rapidly parsed in a single pass and
in isolation. Type checking was, to the extend it allowed it, also possible
in a single pass over the whole translation unit.

The fundamental reason this was possible was that the set of types
were fixed and represented syntactically by keywords such as `int`
or `double`. User defined types in the form of `struct` definitions
were allowed, however the types had to be refered to using
the `struct` keyword and a tag. Because of this, an incomplete
type could be used in a type defintion, and correctly parsed.

In particular, consider these fragments:

.. code-block:: c

  (X)(y)          /* function application */
  (struct X)(y)   /* cast */

There is no need to see the surrounding context. In other words, the
language was context free.

Unfortunately the ANSI committee came along and destroyed this property
by introducing `typedef`. Consequently the first case above is ambiguous,
and can only be correctly parsed if the whole of the previous code
is seen, in case X is type introduced by a typedef.

The loss of context freedom was a serious mistake for C, but for modern
C++ it is an unmitigated disaster. C++ programmers today use a lot of
templates and many libraries are *header file only* and every compilation
of every translation unit has to parse all the header files sequentially,
every time.

However the situation was even worse than that! With templates,
it is not possible to tell if X is a type or not, just be parsing
all the header files. The following example shows why:

.. code-block:: cpp

  template<class T>
  void f() { int x = (T::X)(g); }

If T is instantiated by a class which contains X as a typedef,
then the RHS of the assignment is a cast, if X is a function,
then it is a function application. Without any further information,
the template cannot be parsed at all.

The ISO C++ committee introduced a new keyword to fix this, `typename`:

.. code-block:: cpp

  template<class T>
  void f() { int x = (typename T::X)(g); }
  // its a cast!

If you don't use `typename` then its a function application.

The real situation is worse again because you can also pass
templates as arguments!

There are other cases in C++ where parsing is ambiguous.
The most famous is that it is impossible to tell the difference
in C++ between an declaration and an initialisation:

.. code-block:: cpp

  T f(X);

This could be declaring the function f, returning type T
and accepting type X, or, it could be an initialisation
of the variable f, of type T, to the value X. 

The ISO C++ committee introduced disambiguating rules, but again,
the choice depends on context. Luckily C++ has other, non-ambiguous
ways to achieve the required result, but still, this yet another
serious design fault which makes parsing difficult.

Of course the famous problem with `>>` in templates is well
known, which stems from another serious mistake in ARM C++
using `<` and `>` as brackets, as well as comparison operators.

The need for context to parse C and C++ is not merely a problem
for the compiler, it is a problem for the reader as well.
And it is an even worse problem for the programmer when trying
to refactor code. 

In addition, the C++ committee had a desire when adding new features
to avoid introducing new keywords, so many constructions are introduced
by syntactic forms which are hard to decipher and in many cases
the design is actually flawed because it fails to allow syntact
distinctions to be made which have semantic impact.

The worst example of this is the template specialisation
syntax. Contrary to popular belief, function templates
cannot be specialised, only overloaded, however
class member function can be specialised, but not
overloaded! For example:

.. code-block:: cpp

  template<class T>
  void f(T);
  
  template<class U>
  void f<vector<U>>(vector<U>);

This looks like it is declaring a specialisation of 
the function template f, but it isn't. It is actually
introducing a completely new function which happens
to be defined by a specialistion of the original f.

The new function overloads with the original one,
and since it is more specialised will be selected
by overload resolution. However a real specialisation
has no impact on lookup at all, only on instantiation.
This is the case for classes:

.. code-block:: cpp

  template<class T>
  class X { void f(T); };

  template<class U>
  class X<vector<U>>;

This introduces a specialisation, and by default
the member f is also specialised .. there is no
overloading here. Even if a replacement is defined
for the f, this has no impact on overloading.

The problem is that the committee didn't understand
the difference between these two cases and provided
a syntax in which it is impossible to distinguish them.
Hence, function template specialisations are overloads
not specialisations, because some choice had to be
made given the faulty syntax.

No type checking in templates
-----------------------------

This is a very serious design fault. Templates should introduce
polymorphic types and functions, but they do not, because
they cannot be type checked. Therefore, templates are just
syntax macros, and the result is a disaster.

Recently there was an attempt to solve this problem the
way Haskell does with type classes: the feature known
as `concepts`. Unfortunately the design was rejected and
replaced by a much weaker version known as `concepts-lite`.

If templates could be type checked, this would mean
instantiations would not require type checking:
all instantiations would be guaranteed to be correct.
That also means the instantiation would be entirely independent
of context, and in particular two instantiations with the 
same template arguments in different places would necessarily
be the same type.

Lvalues and references
----------------------

In C, a variable name has two distinct meanings
depending on context. If it is used on the LHS
of an assignment, or as the argument of the addressof operator,
then it represents a storage location. The assignment puts
a value into that location, and the addressof operator finds
a pointer to that location.

In C, the context where a variable name is treated as
refering to a storage location is called an l-context,
other contexts are called r-contexts. The `l` and `r`
refer to which side of an simple assignment it might be.

A variable name is an `lvalue` which means it refers to
a storage location in an lcontext, but the value stored
at that location in an rcontext.

Similar rules apply to, for example, pointer dereferences.
Certain syntactically recognisable expressions in C are
said to be lvalues, others are rvalues. Lvalues can be used
in both lcontexts and rcontexts, in an rcontext the lvalue
degrades to an rvalue. An rvalue cannot be used in an lcontext.

In summary in C, the semantics of certain expressions depends
on a context which is locally syntactically determinate.

The ambiguity is bad, and causes a lot of confusion, but
the disambiguation is possible by simply examining the
expression in isolation and following the rules layed
down in the C Standard.

Unfortunately C++ introduced a notion of references
and reference types and all hell broke loose!
Because a reference is universally an lvalue, but is
also a type, it is not longer possible to determine 
the meaning or correctness of an expression from local syntactic
examination. For example

.. code-block:: cpp

  f(x) = g(y);

would never be allowed in C (after pre-processing), because the
LHS does not have the syntactic form of an lvalue. In C++,
you need to examine the function `f` to see if it returns a non-const
reference to determine if the above code is correct: and that also
means determining the type of `x` because the function `f` could be
overloaded. If we replace `x` with an expression:

.. code-block:: cpp

  f(h(x)) = g(y);

we now have to type the expression `h(x)` which recursively involves
overload resolution for `h`.

This may seem complicated but the situation is much worse.
For a start, the ARM was very confused about overloading
function with reference type arguments:

.. code-block:: cpp

  void f(int);
  void f(int&);
  void f(int const&);
  int x=1;
  f(x); // which f?

What is the type of x? It is an lvalue, but it has type `int`,
but lvalues are replaced by references, so the type should
actually be `int&`. But consider now:

.. code-block:: cpp

  int &x = y;

and clearly `x` now refers to the same store as `y`, so the type
is `int&` but the definition has quite distinct semantics from
an int definition: an int definition creates a new store to
put an int in, the int& definition causes x to refer to
existing store. The types in an expression are the same however,
and that means `f(x)` must call the same overload in both cases.

The ARM got this wrong. The ISO committee debated this issue at length
and resolved it, but they chose the wrong solution. The correct solution
was to throw out the whole idea of reference types: instead a perverted
form of reference types was introduced in which they were just renamed
as lvalue types.

It is legitimate to allow function arguments to be passed by reference,
and this is certainly part of the type information of the function,
but references have no place as types in themselves because they
are not proper type constructors.

A polymorphic type constructor must be combinatorial for 
parametric polymorphism to work. For example for any type T,
the type `*T` makes sense, it is the type of a pointer to T.
The pointer type constructor is properly parametric
because it can be applied to any type, including another pointer 
type.

References are not combinatorial, it is nonsense to take a reference
to a reference. No one would do this in practice in monomorphic code
so it might be excused but for templates. 

If a reference is a type, then a template type parameter could be
set to one, and then all hell breaks loose because it changes,
utterly, the semantic of the template.

.. code-block:: cpp

  template<class T>
  void f() { T x = T(); }

In this template, all is fine provided T has a default constructor.
But what can we say if T is a reference:

.. code-block:: cpp

  f<int&>();

Since references don't have default constructors, we get error.
But consider this one:

.. code-block:: cpp

  template<class T>
  void f(T x, T y) { x = y; }

For a value type T, f does nothing, except perhaps exhibit the behaviour
of an overloaded assignment operator. But if T is a reference
this code has an effect, it assigns the value of y to the
location to which x refers.

In theory, there is no need for references at all. Pointers are perfectly
good enough and pointer calculations are purely functional. They are
first class types and the pointer constructor is parametric.

Introducing references was a serious design fault. It has lead
to introduction of even worse design faults including `decltype`
to handle the problems.

Const
-----

Const is another thing inherited from C and messed up in C++
very badly.

In C, the type syntax makes it seem like you can have a const type.
This is not the case. The syntax is misleading, there are no const types
in C. In C there are pointers to const, and that is all.

It may seem otherwise examining this code:

.. code-block:: cpp

  int const x = 1;
  int const *px = &x;

In C, x has the type int, not const int. Rather, C introduces a new
form of lvalue, a const lvalue. If you take the address of a const
lvalue you get a pointer to const. But as an rvalue, x has type int.

Of course it works the other way too:

.. code-block:: cpp

  *px = 1; // error, const lvalue!

Because px is a pointer to const, a derefernce produces a const
lvalue which can be addressed but not assigned to.

Const lvalues in C cause a problem though because now, the kind
of lvalue is context dependent. In C++ this is true as well.

There is another problem with const: that which is pointed
at by a const pointer need not be immutable because of aliasing.
C introduced the `restrict` keyword to enhance optimisation opportunities
since overlapping array arguments were never allowed in Fortran,
and Fortran remained the premier numerical programming language for
decades (and still is). Restrict disallows aliasing and so a
restricted const pointer, whilst still does not pointer to immutable
store, can be assumed to point at store which doesn't change during
the lifetime of the function.

In C++ all hope is lost when we consider templates. Because both
const and reference are effectively types, the semantics of a template
are utterly indeterminate until it is instantiated. Weird effects
can occur, and be type correct, when instantiating a template
with a const and/or reference type.

Offsets
-------

In C, address arithmetic can be done with casts, and by use
of the `offsetof` macro. The result isn't type safe, but
all useful calculations can be done.

In C++, a type safe version of the `offsetof` macro was introduced,
namely a type *pointer to member*. Unfortunately, the ISO committee
again made a mess of things by insisting on pointers to members working
with virtual functions and classes. As a result, the full calculus is
incomplete.

In principle, if you have a struct nested in another struct,
you should be able to calculate the offset of a member of the inner
struct by adding the offset of the member of the outer struct with
respect to the outer struct, to the offset of the inner member,
with respect to the inner struct, obtaining the offset of the inner
member with respect to the outer struct. Unfortunately, there is no syntax
to do this addition, in part because the calulation cannot be done in
the presence of bases. The problem is, you need to know the layout
of the classes to do the operation: given a pointer to the outer struct,
you can add the outer pointer to member, then the inner one, to obtain
a pointer to the inner member, but the operation isn't associative,
so you cannot add the pointers to members together first.

Of course it can be done with a closure, that is, with lazy evaluation,
but this requires a pointer to member to be an arbitrarily complex
data type with indeterminate storage requirements.

Pointers to member of ordinary non-OO style structs are vitally
important because they are first class projections, and they should
be composable.

Object Orientation
------------------

Adding OO to C gave us the slogan for early C++ as `C with classes`.
It seemed like a good idea at the time, but object orientation
is fraught with peril and it has been established for a long time
that it does not provide a general mechanism for providing abstract
data types. This is due to what is called the `covariance problem`,
which requires the argument of a method to be contra-variant.
Unfortunately to implement a binary operator we require covariance,
and so OO cannot represent even binary operators. It is still useful
when a method has no arguments, that is, for properties, or, when
the arguments are invariant, for example for character device drivers.

No one takes OO seriously in modern C++: most programming uses templates
which is roughly functional programming.

Classes introduced a whole host of bugs. Arrays of value of a derive type
can be implicitly converted to arrays of the base type via the degradation
of array types to pointers to the first member, which then results in
increments and random accesses to the array using the size of the base
type as an offset instead of the derived type.

This is not the only unsound feature introduces. Another well known
example is the ability of a class constructor to export a non-const pointer to 
itself, even if the value is specified as const. This is because in the body
of a constructor the this pointer is non-const, which is required for
storing values in the object.

Another related unsoundness in the type system is that in a constructor body,
the whole of the currently in scope class is visible, including bases.
Unfortunately, with muliple inheritance there is no assurance that all
the bases have been constructed yet.

Worse, the constructor body can invoke a virtual function which would,
after the whole object is built, dispatch to a method of the complete type.
In single inheritance, the virtual table can be built for the base first,
so a dispatch in a derived class constructor will work correctly for that
point in time provided it doesn't dispatch to a method in the current
class and depend on members which have no yet been set.

Unfortunately, with multiple inheritance and virtual bases,
it is not possible to assure the correct virtual table is installed,
because a virtual base can dispatch to an as yet unconstructed derived
class which is not even visible to the current class.  This is known
as sibling dispatch:

.. code-block:: cpp

   struct V { virtual void f()=0; };
   struct D : virtual V { void f(); };
   struct E : virtual V { E() { f(); } };
   struct X : E, D {};

The problem here is that E is constructed first and it calls f() which dispatchs
via its base V to D's override of f. The problem is D hasn't been constructed
yet and so the virtual table in V points f off into thin air, typically
to a run time diagnostic followed by a program abort  which usually says that
a pure virtual has been called. Of course once X is completed the dispatch
would work just fine.

How Felix fixes the problems
----------------------------

Felix fixes all the above problems.

Parsing
+++++++

First, the language is designed so that, with one exception,
all top level constructs, including any file, can be parsed
independently of context. 

All files can be parsed independently of all others, however
the rule for constructions has an exception: if a scope,
including a file scope, opens a syntax module (called a DSSL
in Felix), then the grammar parsed changes from the current
grammar at the point of the open directive.

Opening new grammar is unusual in most code, and when used
in a file it is usually done at the top of the file so it
applies to the whole file. The grammar extensions are scoped
so they cannot be exported from the file, and, if used
inside some scope such as a class, they cannot be exported
from the class.

Syntactic complexity
++++++++++++++++++++

Felix fixes the arcane complexity of C++ by the simple expedient
of throwing out the whole grammar and starting afresh.

Indeed, the grammar in Felix is part of the library, in user space,
so considerable complexity can and is introduced, but there is
an opportunity to design the syntax in a sane manner.

One specific feature that should be noted is that in Felix
there are no keywords. Felix uses a GLR+ extensible parser
and recognises identifiers as keywords only in a context sensitive
manner. There are a lot of such context sensitive keywors but
they can be designed into the grammar with impunity because
most of the time they are effective only in the context for which
they're introduced, and where there is an conflict, Felix provides
a special lexical form to force recognition of an identifier.
For example:

.. code-block:: felix

  var var = 1;
  n"var" = var;
  println$ var;

Here, the first `var` is in a context where it is treated as a keyword whilst
the second is not, so it is treated as an identifier. The third use would be
treated as a keyword so we use the special lexeme which forces interpretation
of an identifier. The fourth and fifth contexts treat `var` and an identifier.

Lvalues and references
++++++++++++++++++++++

Felix has no concept of a reference, it just uses pointers.
There is only one allowed kind of lvalue, namely a variable name,
and only one operation on it, namely to take its address.

In principle even this is not the case: in Felix a variable
definition of a name `x` is actually a pointer which has to be
dereferenced to get a value, but the language does this automatically
and the use of the addressing operator merely inhibits this 
behaviour.

Felix uses the following to store values:

.. code-block:: felix

  var x = 1;
  var px = &x;
  px <- 2;
  storeat (px, 2);
  &x <- 1;
  x = 1;

The `<-` symbol is an infix operator which invokes the `storeat` procedure,
which is the *only* way to store a value supported by the language.
The last line is an assignment, but that is actually syntactic sugar
for a call to the storeat procedure to the address of the LHS,
and it cannot be used for components of a product type, only simple
variables.

To store values into the component of a product type, Felix uses
first class projections which apply to pointers. For example:

.. code-block:: felix

  struct X { x:int; };
  var a = X (1);
  &a.x <- a.x + 1;

In the last line, the RHS symbol `x` is actually a value projection of type:

.. code-block:: felix

  X -> int

Projections are first class functions. There is no "member access syntax" either.
Instead, operator dot (.) is unniversally just reverse application. Therefore
you can write this as well:

.. code-block:: felix

  x &a <- x a + 1;


Here you can see `x` is overloaded so that as well as the type of the value
projection, there is also an overload for pointers:

.. code-block:: felix

  &X -> &int

This overload calculates the address of the member and so now you can
store in the member.

Const
+++++

Felix, like C++, has a const pointer type, however unlike C++ there
are no references (they're not needed, see above). And there is
no confusion about const types, there is no such thing in either
language but in C++ the syntax suggests there is. Instead in Felix
we have read only and write only pointers:

.. code-block:: felix

  x &>a <- *(x &<a) + 1;

The LHS address of operator returns a write only pointer, whilst the
RHS operator a read only pointer. Read/write pointers use the plain &
operator and the type is a subtype of both read only and write only
pointers.


Object Orientation
++++++++++++++++++

Felix fixes this problem by annihilation. There is no OO in Felix.



