==================
Subtyping in Felix
==================

Felix supports certain implicit conversions in certain contexts
which are considered subtyping coercions. The context currently
supported is the coercion of an argument to the type of a parameter
in a function application or procedure call.

Context of Implicit Coercions
=============================

Felix does *not* support implicit coercions in simple assignments
or local variable initialisations, even though in some sense
initialisation at least is somehow equivalent to binding 
a parameter to an argument.

The reason is that whilst for a local variable initialisation,
there is space to write the coercion explicitly in a neutral
manner, the argument and parameter of a function call are
lexically separated. 

The rule for selection of a function from an overloaded set
in the presence of implicit coercions is a generalisation
of the usual subsumption rule for overloads of polymorphic
functions, namely, the selection of the most specialised
function from the set which matches the argument.

A parameter type A is more specialised than another B
if A is a subtype of B.

This imposes a strict coherence constraint on subtyping
coercions. In particular if A is a subtype of B, and
B a subtype of C, then A must be a subtype of C as well,
and the composition of the subtyping coercions from
A to B and then B to C must be semantically equivalent
to a subtyping coercion from A to C. 

Furthermore, any two subtyping coercions from A to B
must be semantically equivalent.

The transitivity rules has two vital consequences.
The first is that the compiler must be able to calculate
a composite subtyping coercion from A to C via B,
if there is a coercion from A to B and from B to C.
The second is that the programmer should take care
that if in such circumstances a coercion is also
given from A to C, it is a semantically equivalent
to the composite.

Generally, the compiler must be free to pick
any composition as the implementation of a coercion,
and we can view the picking of an efficient composition,
such as the single user defined coercion from A to C
as an optimisation.

Standard Subtyping Coercions
============================

Record Coercions
----------------

Record support two a stage coercion rule. The first
rule is called a `width` coercion and allows fields
of a record to be thrown away.

The second stage, called a `depth` coercion,
permits the field values
to be individually coerced covariantly. In other
words a coercion from a subtype to a super type
consists of discarding some fields, and then
applying subtyping coercions to the values of
the remaining fields.

The justification of the width coercion rule is this:
if a function requires a record with a certain set
of fields, the supplying a record with more fields
is acceptable, because the function ignores
them anyhow.


Tuple Coercions
---------------

Felix supports covariant depth coercions of tuples.

We do not support width coercions, however.
The reason is that the programmer would be surprised
if components of a tuple magically disappeared
at random just to match a function signature.

In particular, since in Felix we identify a tuple
of length one with that element, allowing width coercions
would be tantamount to allowing a tuple to be supplied
if `any` of its components could be coerced to a function
parameter.

Array Coercions
---------------

Felix also supports covariant depth coercion
of arrays with a constraint that the same coercion
must be applied to each element of the array.

We do not support implicit with coercions because
the programmer might be surprised if an array
was magically truncated.

Polymorphic Variant Coercions
-----------------------------

Polymorphic variant coercions also support
two stage coercions, in reversed order:
for the first stage we can covariantly coerce
the constructor arguments, and in the second
stage add additional constructors.

The justification of the width coercion rule
is this: if a function requires a polymorphic
variant from a certain set of cases which it
analyses, then the analysis will completely
handle less cases.

Function Value Coercions
------------------------

Function values are coerced contravariantly
on their domain and covariantly on their
codomain.

The justification is as follows. Suppose we have
a function that accepts another function as an argument.
When we apply that function to a value, it must handle
all the argument values that the function can throw at it.
Therefore the domain of the function supplied must be a
supertype of the domain of the parameter.

Conversely, it is fine if the supplied function returns
a more restricted set of values than is required
in the context in which it is supplied, thus, the
codomain of the argument can be a subtype of the
codomain of the parameter.

Machine Pointer Coercions
-------------------------

Felix has three core pointer kinds: read-only pointers,
write-only pointers, and read/write pointers. Read/write
pointers are considered subtypes of read-only pointers
and write-only pointers with an invariant target type.

In theory, read-only pointers should be covariant
and write only pointers should be contravariant,
so that read-write pointers are invariant.

Top and Bottom
--------------

In a subtyping lattice, it is usual in the theory
to have a type `top` which all types are subtypes of,
and, a type `bottom` which is a subtype of all other types.

Felix has both these types. The type `any` is the top type
and is defined by the equation:

.. code-block:: felix

    typedef any = any;

It is, clearly, a recursive type, since it refers to itself.
Felix uses this type for functions which never return, such as `exit`.
In principle, `any` should unify with any type, and every type
should be a subtype of `any` but Felix currently does not
implement this.

Similarly, Felix has a type `void` which is the bottom type
defined by

.. code-block:: felix

    typedef void = 0;

the sum of no units. There are no values of type `void`. 
In Felix, a function returning void is a procedure,
which returns control but no value.

In principle, void is a subtype of all other types,
however Felix does not do this. Instead, void unifies
only with void, and otherwise unification fails.
Were the theoretical subtyping rule applied,
a function with a void parameter would accept an
argument of any type. It would do this by simply
throwing out the argument. However we do not currently
support that.

Functions of void certainly exist, in the category of
sets there is indeed a unique function from void to
every other type: void is simply the empty set,
and a function from the empty set to any other set
is modelled by a set of pairs which happens to be empty.

Felix does in fact use some internal tricks where
a constant constructor, that is, one with no arguments,
is modelled as an injection from void. One can argument
that, in fact, all literal values, are in fact 
precisely function from void to a singleton type 
containing the value only, as a subtype of the type
of the literal. But we don't do that, except as
an internal trick.

We may relax these rules later and explicitly
support any and void as top and bottom elements
with corresponding coercions. Unfortunately the
theory of types is based on functional programming
model and fails to properly account for effects.
Because of this, it is dangerous to provide the full
theory, because we would be out of types for
procedures and exits, and we would allow dangerous
compositions which had side effects functions are
not permitted to have.


User Defined Coercions
----------------------

Felix currently supports a very limited set of coercions
which can be defined by the user. The user defines a function
named `supertype` which is a coercion from its domain, the subtype,
to its codomain, the super type. For example:

.. code-block:: felix

    supertype (x:int) => x.long;

says that `int` is a subtype of `long`. This means a function
with a long parameter can be called with an int argument.
The domain and codomain must be monomorphic nominal types.
This requirement may be relaxed in future versions.
The compiler does `not` find composite coercions so
technically to retain coherence the user is required to
define all composites.

Discussion
==========

Felix has certain rules which could be represented
by coercions but, instead, are represented as identities.
In addition, it has some rules which appear to the user
as if they were identities but which are, in fact,
coercions!

In Felix, a record of all anonymous fields is a tuple,
a tuple of all components of the same type is an array,
and an array of one element is that element. These are
identities of the language, not coercions. Although they
appear as a kind of subtyping rule: an element is a special
case of an array which is a special case of a tuple
which is a special case of a record, in fact, these special
cases are only notional.

On the other hand, Felix allows a function to be used when
a function value is required, and that is real implicit coercion.
Indeed, unlike some other languages there are contexts in which
projections and injections can also be used as function values.

This case is a real coercion. Not only does the compiler
use quite distinct terms internally, but the generated
C++ code is also quite distinct. For example, a function in
Felix in general form is represented by a C++ class, whereas
a function value is a pointer to a heap allocated object
of that class type, completely different kinds of entity.

Nevertheless coherence concerns exist, especially mixing
these morphisms with subtyping conversions. It may surprise
a user that this is a match:

.. code-block:: felix

    fun f(p: int * long) => ...
    .. f (field="Hello", 1,2) ..

assuming that we have a coercion from int to long, however the
application of f here fails:

.. code-block:: felix

    fun f(p: int * long) => ...
    .. f (1,2) ..

even though we just dropped the field of string type, which
was thrown out by the record coercion anyhow, because now,
the argument is an array of two ints, and the same coercion
must be applied to all elements, and no coercion exists to
convert the array of two ints to a non-array tuple.
With the string field in place, distinct coercions were
allowed.

Such surpises arise in most languages. The most common
is more annoying than surprising: one wants a value of 
the type of some entity which, in the language, is
only a second class citizen. For example modules in 
Ocaml (until recently!) or type classes in Haskell.

By comparison, in many dynamically typed languages a lot
more entities are first class, of necessity. This is because
the languages are traditionally interpreters, and the first
class values must exist for the interpreter to work at all.
This is an often overlooked reason why programmers like
dynamic languages: it is not, as many claim, that they dislike
static typing as such, but because static type systems are
extremely weak by comparison. The extensibility of a large
set of Python programs by dynamically loading user extensions
to a framework are simply impossible without run time 
type checks.

Another overlooked features is that consistent and well
documented run time type checks actually facilitate dynamic
extension. By comparison whilst the same effect can always
be obtained in a statically typed language, the programmer
of such a system has to reinvent the wheel to obtain 
dynamics. Python, for example, has a well specified
layout for module lookup tables and for type objects
which greatly simplify the task of dynamic extension
whilst also constraining the kinds of extensions that
can be provided to those that are readily supported by
the existing framework.

It is indeed quite suprising to find that completely open
nature of how dynamics can be implemented in static languages
is a severe impediment to reasoning about such systems,
not an advantage as often claimed. It is not uncommon,
for example, for programmers of strongly typed static
languages to resort to parsing strings to implement
dynamics.

It is disappointing, for example, that in Felix whilst
the type laws

.. code-block:: felix

    3 = 1 + 1 + 1
    int * int = int ^ 2

hold, the law

.. code-block:: felix
 
    int + int = 2 * int

does not. In fact, the standard representation of sum types
and unions does in fact use a pair consisting of a an integer
tag and a pointer to the constructor argument, there are
also special cases for unions which use more compact and
efficient representations, which thereby break the law
at the representation level. For example the representation
of an list uses a single pointer, not a pair, with the
NULL value representing the Empty case and a non-NULL 
value representing a non-empty tail. Similarly, a 
standard C pointer which could be NULL, is in fact
the representation of the type:

.. code-block:: felix

    union Cptr[T] = nullptr | &T;

which allows Felix to use possibly NULL pointers from C
directly in the language without any binding glue.
Similarly the representation of `int + int` is optimised
to a single pointer with the discriminant tag in the low
bit of the pointer. Its a nice trick for performance but
the C code is not the same as the representation of
`2 * int` even though it is isomorphic.

It may seem tempting to introduce many identities and
representations as subtyping coercions but the unfortunate
fact is that such apparent simplification actually ends
up breaking the coherence rule for subtyping and thus
is inadmissable. No matter what representations you choose,
some coercions will always be value conversions rather than
simply type casts.

Dynamic languages, on the other hand, rarely have this
problem because all the conversions are run time value
conversions: in some sense, dynamic systems are, in fact,
more coherent than static ones.




