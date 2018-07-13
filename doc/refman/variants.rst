Polymorphic Variants
====================

Polymorphic variants are a kind of open union type.
A value is formed by using a globally unique name preceded
by a backquote character and followed by a value:

.. code-block:: felix

  `Some 42;

This variant has the type

.. code-block:: felix

   `Some of int

A polymorphic variant type is given by a set of unique constructor
names and types:

.. code-block:: felix
 
  typedef option[T] = (
    | `Some of int
    | `None
  );

and such types obey both width and depth subtyping rules.

Width subtyping says that a type A with a subset of the constructors
of a type P is a subtype of P, depth subtyping extends the rule
to also allow the arguments of A's constructors to be subtypes
of those of P. In other words, subtyping is covariant.

Felix applies subtyping rules automatically when applying
a function or procedure to a value:

.. code-block:: felix

   proc show[T with Str[T]] (a: option[T]) {
      match a with
      | `Some v => println$ "Some " + v.str;
      | `None => println$ "None"
      endmatch;
   }
   show (`Some 42);

However a coercion is required for assigments, including
initialisations:

.. code-block:: felix

  var x = `Some 42 :>> option[int];

Since pattern variables are also set by initialisation,
a special form of pattern is required to specify the 
variable type: because you can consider a pattern
an "inside-out" form of code, the pattern is written
as a backwards coercion, with the type first. 
This is illustrated in the next example.

Polymorphic variants are weaker than unions in that they
offer less safety guarantees because the set of constructors
and their arguments are open. However this allows extremely
powerful but reasonably well constrainted extension models,
demonstrated in the example below which uses the technique
known as open recursion:


.. code-block:: felix

    typedef addable' [T] = (
     | `Val of int 
     | `Add of T * T
     )
    ;

    fun show'[T] (show: T->string) (x: addable'[T]) => 
     match x with
     | `Val q => "Val " + q._strr
     | `Add (a,b) => show a + " + " + show b
    ;

    typedef addable = addable'[addable];
    fun show(x:addable): string => show' show x;

    var x = `Add (`Val 1, `Val 2);
    println$ show x;

    typedef subable' [T] = ( 
    | addable'[T]
    | `Sub of T * T
    );

    fun show2'[T] (show2: T->string) (x:subable'[T]) =>
      match x with
      | `Sub (a,b) => show2 a + " - " + show2 b
      | (addable'[T] :>> y) => show'[T] show2 y
    ; 

    typedef subable = subable'[subable];
    fun show2 (x:subable): string => show2' show2 x;

    var y = `Add (`Sub (`Val 1, `Val 2), `Val 3);
    println$ show2 x; // <============
    println$ show2 y;

In this example we define first a type of expression which allows
just values and addition. We provide a function to show such 
expressions as a string. We do this by first providing a type
with an unspecified parameter, and then use 
a fixpoint operation (self-reference) to bind the parameter to 
the type. The pattern for the function is similar: we first
provide an open function which takes an argument function to
show the value of the type of the parameter, and then
fix the function to just addable types by applying the function
to itself.

Then, to extend the system to work with subtraction as well,
we define a new type by adding a subtraction term to the
open form of the addition term, and then again fixate
the type. Similarly, the open form of the show function
handles the new subtraction term itself but delegates
to the open form of the function handling addition.
Then we fixate that function by applying it to itself
to obtain a closed function.

The power of this method is seen in the line indication
with a commented arrow: the closed show2 function which
works with expressions containing subtractions also works
with the older, legacy expressions containing only addition.

It works because of covariant subtyping: the closed terms
with addition are subtypes of the closed terms that
also include subtraction.

The vital importance of this technique cannot be overstated.
Unlike object orientation, which requires methods to have
contravariant argument types, open recursion is covariant. It therefore
supports Meyer's Open/Closed principle whilst, despite his
intentions, object orientation does not.



