Overloading System
==================

Felix provides two forms of overloading: ad-hoc overloading is similar
to C++, whereas type class based overloading is a two stage process
where functions are first bound as if parametrically polymorphic,
and then the best instance is selected by a similar algorithm.

Unification
-----------

The primary stage of overload resolution works by examining the set
of functions with the required name which are visible at the point
of call. Felix uses a technical variant of the unification algorithm
which first calculates if the argument type is a specialisation
of the parameter, if so, retaining the most general unifier (MGU).

Specialisation is the same as unification except that the type variables
of the parameter must be disjoint from those in the argument, which is
ensured by alpha-conversion is necessary. The parameter's type variables
form the dependent set, and those of the argument the independent set.
The MGU must contain assignments to each of the variables of the dependent set.
A specialisation of the function is then calculated by replacing the dependent
type variables with the value assigned to them.

If, from the set of available function signatures, there is more than one
match, then the algorithm tries to find the most specialised. It does this
by removing from the set any signature less specialised than some other.

If there are two or more signatures left at the end of this phase,
two further steps are followed to resolve the ambiguity.

First, if the function is a higher order function (HOF) called with 
several arguments, the next argument is considered: functions which
do not have a second argument are discarded and those for which there
is a non-matching second argument are also rejected, of those that
remain, we repeat the primary unification step. In practice, this
is done simultaneously.

Constraints
-----------

Next, of the remaining functions, constraints are applied to reduce
the set of candidates further. At this point if two or more functions
remain the call is deemed ambiguous. If there is exactly one remaining
function that is the selected function.

Finally if there are now available functions, the overload resolution
fails, however this is not the end of the story. Felix has several ways
to continue. The primary recovery mechanism is to consider functions
in the next outermost scope. The overload resolution process is repeated
until we either find as match, find an ambiguity, or run out of 
scopes by reaching the top level scope. Note that this is *different*
to C++ in that functions are visible from outer scope without the need
to inject them.

If we run out of scopes, the lookup machinery has a number of specialised
ways to try again which are beyond the scope of this description to 
elaborate, however one is to prefix `_ctor_` to the name of the function
and try again. This means if you apply a type to a value, the primary
overload fails (because a type with that name is found, instead of a function
so the set of candidates is empty). The effect of this rule is to search
again for conversion functions named as `ctor` in the user code.

Another method for a retry is to consider the parameter names, if the
function is called with a record. Felix also supports default arguments.

Type Class virtual instantiation
--------------------------------

Type class virtual functions are instantiated purely with unification,
the tricks, constraints, and mult-level scoping considerations
do not apply here: it is a purely a matter of specialisation.

Subtyping
---------

When the unification engine allows an argument with a type which
is a subtype of the parameter, the mismatch is detected by the
lookup machinery and a coercion is inserted for *each* of the functions
in the overload resolution candidate set. Because this is done
first, an exact match and a match obtained by subtyping are considered
equal matches and this results in an ambiguity. Unlike C++, at this time
Felix has no way to weight matches based on "how much subtyping" is done
although the resolution would be precisely the same as the primary
method for selecting the "most specialised" for structural types,
there is no clear way to do this for nominal types, however. 
