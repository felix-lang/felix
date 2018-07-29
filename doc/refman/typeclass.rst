Type Classes
============

A type class in Felix is similar to Haskell type classes.
It provides a form of existentially delimited constrained polymorphism.

The intent of type classes is to provide a consistent notation
for algebraically related nominal types. For example here is the 
library definition for an equivalence relation:

.. code-block:: felix

  class Eq[t] {
    virtual fun == : t * t -> bool;
    virtual fun != (x:t,y:t):bool => not (x == y);

    axiom reflex(x:t): x == x;
    axiom sym(x:t, y:t): (x == y) == (y == x);
    axiom trans(x:t, y:t, z:t): x == y and y == z implies x == z;

    fun eq(x:t, y:t)=> x == y;
    fun ne(x:t, y:t)=> x != y;
    fun \ne(x:t, y:t)=> x != y;
    fun \neq(x:t, y:t)=> x != y;
  }

The class specifies a virtual function named `==` which is intended to
measure equivalence of values. There is another virtual function
which measures inequivalence, it defaults to the negation of equivalence.

To specify equality for int we can write:

.. code-block:: felix

  instance Eq[int] {
    fun eq: int * int -> bool "$1==$2";
  }

which delegates the calculation for Felix int type to C++
`operator ==`. We can use a type class like this:

.. code-block:: felix

   fun alleq[U with Eq[U]] (x:U, y:U, z:U) =>
      x == y and y == z
   ;

The effect of the `with Eq[T]` clause, which is sometimes
called a type class constraint, is two fold: first, the
signatures of the type class are injected into the scope
of the function, replacing the type class type variable
`T` with the type expression in the constraint in the same
position, which happens to be `U` in this case. This allows the
function to be type checked and overload resolution performed.

Now suppose you use this function:

.. code-block:: felix

   println$ alleq(1,1,1);

then overload resolution returns an MGU in which `T` is assigned
the type `int`. What happens now is that the system searches for
all the instances of `Eq` and tries to find one for which the
type argument in the position corresponding to T is `int`.
Then, the binding to the type class virtual `Eq[T]::==` is replaced
by the instance override `Eq[int]::==` thereby invoking a version
of `==` which is specialised to type `int`.

In general Felix monomorphises all code after polymorphic binding
is complete by starting with the program roots, which are necessarily
monomorphic, and recursively descending into polymorphic functions
specialising them completely to monomorphic versions as it goes.
Thus paraemtrically polymorphic functions are monomorphised for each
use and type class virtuals are replaced by actual instances.

Only functions marked `virtual` can be overriden in an instance,
the other functions of a type class are dependent on these 
functions and do not need to be overriden. If a default is given
for a virtual function, overriding it is optional. Overrides
are only required in an instance if they're used.

Instances can be polymorphic too, an override in a polymorphic
instance matches by the usual rules by selecting the most
specialised instance or reporting an ambiguity. The significant
difference here is that the specialisation rules apply to the
instance as a whole *not* to the individual functions.

If an instance cannot be found, an instantiation error is reported,
this is not a type error, its a missing instance which can be
fixed by the programmer adding an appropriate instance.
This is similar to a missing symbol at link time, fixed by
linking an extra library containing it.

Instances are independently type checked and bound so type
errors are not possible during monomorphisation.

Felis also allows virtual types in type classes, these types
must be specified in instances. Virtual types can participate
in ad-hoc overload resolution, but the specified per instance 
type cannot. This is because the substitution occurs during
monomorphisation *after* ad-overload resolution is complete.
However type class overloading is performed *during* monomorphisation
so the instance types can be used at that point.

Type class provide the only way to do polymorphic recursion.
Since Felix does not do boxing, these recursions are implemented
by expansion and so must terminate.








