Pattern Matching
================

Felix provides an advanced pattern matching system which
includes generic patterns and user defined patterns.

Pattern matching provides a way to decode a data structure
by providing an image of the type with "holes" in it which
are indicated by variables. Provided pattern matches the value
the variables take on the value of the part of the type
which is missing.

For products, pattern variables are projections of the
value, possibly chained together in reverse order.

For sum types including variants, the argument of
the constructor which created the value is extracted,
after checking the value was indeed made by the
corresponding injection function. If not, the next pattern
is tried.

A pattern match over a product type is said to be irrefutable
because it cannot fail after static type checking; however
a pattern it is included in may fail, and a pattern matching
a component may also fail.



