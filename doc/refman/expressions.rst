Expressions
===========

Expressions in generally used to perform calculations and construct
values. Because Felix has a user defined grammar, there are many
expression forms which reduce to function applications. In turn,
since functions are not permitted side-effects, with some caveats
expression forms can be regarded as referentially transparent.

The two main caveats are generators and impurity.

When an expression contains a direct generator application,
it is lifted out of the expression: the application is replaced
by a variable which is initialised before the expression is evaluated.
After the lift and replacement, the remaining expression may be free
of effects. However, generators have the same type as functions,
so if the application is indirect, for example the application
of a closure, Felix doesn't know if it is a generator or function
and may or may not lift it out.

Some functions may depend on variables and indeed, and expression
can contain variables. Since the evaluation is side-effect free the
variable cannot change during the evaluation of the expression.
But it can change in a loop so that a subsequent evaluation
returns a different result. Of course the most trivial case
is when the expression is nothing more than a variable, such
as a loop control variable, in which case we'd be surprised
if the value didn't change!



