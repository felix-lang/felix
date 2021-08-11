==============
Open Recursion
==============

Open/Closed Prinicple
=====================

One of the most fundamental principles of programming languages
is that the language should support some kind of `module` which
is simultanously open for extension, yet also closed so it may
be used without fear changes will disrupt usage.

This principle was clearly stated by Bertrand Meyer in his
seminal work "Object Oriented Software Construction".
It was a key motivator for the idea of a class which
provided a closed, or `encapsulated` resource with 
a closed set of well specified methods to manipulate it,
whilst at the same time being available for extension
via inheritance.

As it turns out this idea fails because identifying a module
with a single type is the wrong answer. Never the less the
core concept is very important.

The Hard Working Programmer
---------------------------

An unenlightened programmer is asked to provide a term representing
an expression which can perform addition on expressions. This is the 
type:

.. code-block:: felix

    typedef addable = (
     | `Val of int 
     | `Add of addable * addable
     )
    ;

and here is the evaluator:

.. code-block:: felix

    fun eval (term: addable) =>
      match term with
      | `Val j => j
      | `Add (t1, t2) => eval t1 + eval t2
    ;

This solve the problem quite nicely. Unfortunately the
client asks for an extension to include subtraction.
The programmer used `copy and paste polymorphism`
to get this type:

.. code-block:: felix

    typedef subable = (
     | `Val of int 
     | `Add of subable * subable  
     | `Sub of subable * subable
     )
    ;

and here is the new evaluator:

.. code-block:: felix

    fun eval2 (term: subable ) =>
      match term with
      | `Val j => j
      | `Add (t1, t2) => eval2 t1 + eval2 t2
      | `Sub (t1, t2) => eval2 t1 - eval2 t2
    ;

This seems reasonable, we still have the old addable type,
but the modifying the original code in your text editors
is a pretty lame way to go: what happens if there is a bug
in the original routine? Now you have to remember to fix
both routines.

Would it surprise you if the client now wants to multiply
as well?

The Lazy programmer
-------------------

The smart programmer writes the same addable routine
as the stupid programmar. But the smart programmers is not
surprised when the client wants and extension. The smart
programmer knows the client will want another one after that too.

So the smart programmer writes this:

.. code-block:: felix

    typedef addable'[T] = (
     | `Val of int 
     | `Add of T * T
     )
    ;

    fun eval'[T] (eval: T-> int) (term: addable'[T]) : int =>
      match term with
      | `Val j => j
      | `Add (t1, t2) => eval t1 + eval t2
    ;

    typedef addable = addable'[addable];
    fun eval (term:addable) : int => eval' eval term;

Now to see why this is a really cool solution:

.. code-block:: felix

    typedef subable'[T] = (
     | addable'[T]
     | `Sub of T * T
     )
    ;

    fun eval2'[T] (eval2: T-> int) (term: subable'[T]) : int =>
      match term with
      | `Sub (t1, t2) => eval2 t1 - eval2 t2
      | (addable'[T] :>> y) => eval' eval2 y
    ;

    typedef subable = subable'[subable];
    fun eval2 (term:subable) : int => eval2' eval2 term;

What you see here is that there is no code duplication.
The new subable' type extends the old addable' type.
The new eval2' routine calls the old eval' routine.

This is the extension required by the open/closed
principle. On the other hand, by making these parametric
entities refer to themselves we fixate them to obtain
a recursive closure. 

Open Recursion
==============

The method shown above is called `open recursion`.
In its simplest form above it requires polymorphic variant types
and higher order function.

With this technique, we make flat, linearly extensible
data types by using a type variable parameter in the type where would
normally want recursion. Similarly in the flat function,
we use a function passed in as a parameter to evaluate
the values of the type of the type variable.

The flat forms are extensible, so these type are open.

But when self-applied, the types become closed
and directly usable.

So the technique provides a method to define a type with
a discrete number of cases, and an an evaluator for it,
and to extend the type to one with more cases, without
impacting uses of the original type, and critically,
without repeating any code.

Subtyping and Variance
======================

Its important to understand why the technique above
works, but an object oriented solution does not.

What you may not have realised is that this works:

.. code-block:: felix

    fun f(x:addable) => eval2 x;

What? Yes, addable is a subtype of subable. First, it is a `width
subtype`, because addable has less cases. But that is not enough.
As well, the arguments of the constructors are subtypes as well.
Because they, too, have less cases. This is called `depth subtyping`.
It applies recursively, and the subtyping is said to be `covariant`.

Object orientation cannot do this, because method arguments
in derived classes must be `contravariant` whereas we want
them to be `covariant`. You would like to do this:

.. code-block:: c++

    class Abstract {
      public: virtual Abstract binop (Abstract const &)const=0;
    };

    class Derived : public virtual Abstract {
      public: Derived binop (Derived const &)const;
    };

where you see because the argument of the binop method has varied
along with the derivation direction, it is said to be covariant.
The problem is, the argument of a method must be either `invariant`
meaning the same type as in the base, or `contravariant` meaning
a base of the base! The return type is covariant, and that is allowed
but covariant method arguments are unsound and cannot be allowed.

You can do this:

.. code-block:: c++

    class Derived : public virtual Abstract {
      public: Derived binop (Abstract const &other)const {
        Derived *d = dynamic_cast<Derived*>(&other);
        if (d) { ... }
        else { .. }
      }
    };

But how do you know you covered all possible derived classes
in the downcast? You don't. If someone adds another one,
you have to write code for it, and this breaks encapsulation.

The simple fact is OO cannot support methods with covariant
arguments which restricts the utility of OO to simple types
where the methods have invariant arguments. OO is very good
for character device drivers, because the write method
accepts a char in both the abstraction and all the derived
classes: it is an invariant argument.
  
Mixins
======

It is clear from the presentation that any number of extensions
can be added using open recursion in a chain. This means you can
form a whole tree of extensions with subtyping relations from
the leaves up to the root. Lets make another extension:

.. code-block:: felix

    typedef mulable'[T] = (
     | addable'[T]
     | `Mul of T * T
     )
    ;

    fun eval3'[T] (eval3: T-> int) (term: mulable'[T]) : int =>
      match term with
      | `Mul (t1, t2) => eval3 t1 * eval3 t2
      | (addable'[T] :>> y) => eval' eval3 y
    ;

    typedef mulable = mulable'[mulable];
    fun eval3 (term:mulable) : int => eval3' eval3 term;

Its the same pattern as subable of course. The question
is, can we combine this with subable, so we can do
addition, subtraction, and multiplication?

.. code-block:: felix

    typedef msable'[T] = (
     | subable'[T]
     | mulable'[T] 
     )
    ;

    fun eval4'[T] (eval4: T-> int) (term: msable'[T]) : int =>
      match term with
      | (subable'[T] :>> y) => eval2' eval4 y
      | (mulable'[T] :>> a) => eval3' eval4 z
    ;

    typedef msable = msable'[mslable];
    fun eval4 (term:msable) : int  => eval4' eval4 term;

The problem here is that both subable' and mulable' contain
the case for Add and Val. You will get a warning but in
this case it is harmless (because it is the same case).

Here's some test code:

.. code-block:: felix

    val x = `Sub (`Add (`Val 42, `Add (`Val 66, `Val 99)), `Val 23);
    val y = `Mul (`Add (`Val 42, `Mul (`Val 66, `Val 99)), `Val 23);
    val z = `Sub (`Add (`Val 42, `Mul (`Val 66, `Val 99)), `Val 23);

    println$ eval2 x; // subable
    println$ eval3 y; // mulable

    println$ eval4 x; // subable
    println$ eval4 y; // mulable
    println$ eval4 z; // msable
    
Note that eval4 works fine on x and y as well as z!

 
