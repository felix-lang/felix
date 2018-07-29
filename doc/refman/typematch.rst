Type Matches
============

Syntax
------

Felix has a special type expression known as a type match,
it is the type system analogue of a match:

.. code-block:: felix

  typedef fun dom (T:TYPE):TYPE =>
    typematch T with
    | ?D -> _ => D
    | _ => 0
    endmatch
  ;

The type function dom above finds the domain type or a function
type, if the argument is a function type, otherwise it returns 
the void type 0. Note that dom does not preserve structure
and so is not a functor.

An identifier in a type pattern is assumed to be a type name
unless it is prefixed by `?` which indicates a pattern variable.

Semantics
---------

The rule for reduction of a type match is non-trivial!

First, the algorithm compares the match argument type with
the pattern of the first branch. If it matches, it returns
the RHS of the branch, replacing any occurences of the 
pattern variables with the parts of the argument that they 
have bound to.

If the argument *can never match* then the first branch is discarded
and we proceed to the next branch. If there are no branches left the
typematch fails.

If the argument *can possibly match in the future* but doesn't at
present, the whole type match is returned (minus any branches which
have been discarded).

Match is done by unification. First, we look to see if the argument
is a specialisation of the pattern, this includes equivalence.
If so we have a match. If not we determine if the pattern unifies
with the argument. If not, the argument can never be a specialisation
of the pattern, even after type variables are replaced in the argument.
Otherwise, it may become a specialisation in the future.

For example:

.. code-block:: felix

  typedef X[T] = 
    typematch T with
    | ?D -> ?C => 1
    | _ -> 0
    endmatch
  ;

As written, the argument T is not a specialisation of the first pattern
`D -> C` but it could become one if, for example, we set T to `int->long`.
In that case the most general unifier (MGU) would be the pair 
`D<-int`,`C<-long`. Therefore, reduction is deferred.

Overload resolution cannot proceed with unreduced type matches.
Therefore, it attempts to remove all type matches and fails
if at least one cannot be reduced away.







