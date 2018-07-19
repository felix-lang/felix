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

Applications
++++++++++++

Felix has a large number of application operators, from
highest precedence to lowest:

.. code-block:: felix

  #f   // constant evaluator: means f ()
  a.f  // operator dot: reverse application, left associative
  f a  // operator whitespace, left associative
  f$a  // operator dollar, right associative
  a|>f // operator pipe apply: reverse application, left associative

There are also two special combinations:

.. code-block:: felix

   a*.f  // means (*a).f
   a&.f  // means (&a).f

The rich collection of operators is indended to reduce the
need for parentheses.

Another application for binary operator is

.. code-block:: felix

  a `(f) b // means f (a,b)

Let forms
+++++++++

A let form allows an expression to be factored:

.. code-block:: felix

  let p = expr1 in expr2

for example:

.. code-block:: felix

  let x2 = x * x in
  let y2 = y * y in
    sqrt (x2 + y2)

Another let form defines a local function:

.. code-block:: felix

  let fun sq(x:int) = x * x in 
    sqrt (sq x + sq y)

Sets
++++

Lists, arrays, and other data structures can be viewed as sets
and support the `in` or `\in` (spelled `\\in`) operator.

Logic
+++++

Felix uses the operators `and`, `or`, `xor`, `implies` as standard
binary operators for logical conjunction, disjunction, disjoint disjunction,
and implication. The unary operator `not` is used for negation,
and we have the constants `false` and `true`.

There are also two special short cut forms: `andthen` and `orthen`.
The second argument of these function is a unit function returning
a bool, *not* a value of boolean type.


Comparisons
+++++++++++

The usual comparison operators are available along with TeX identifiers:

==================== ==================
operator             numeric semantics
==================== ==================
==, \\eq              equality
!=, \\ne              inequality

<, \\lt               less than
<=, \\le              less or equal
>, \\gt               greater than
>=, \\ge              greater or equal
==================== ==================

Arithmetic
++++++++++

The usual operators:

==================== ==================
operator             numeric semantics
==================== ==================
\+                   addition
\-                   subtraction
\*                   multiplication
/                    division
%                    remainder
-, neg               negation
==================== ==================


Bitwise Operations
++++++++++++++++++

The usual C operators spelled differently: note
for numeric types these operations only apply to
unsigned integers.

==================== ==================
operator             numeric semantics
==================== ==================
\|                   bitwise or
\^                   bitwise exclusive or
\&                   bitwise and
~                    ones complement
==================== ==================



Addressing
++++++++++


Composition
+++++++++++

There are two composition operators for functions,
both are left associative:

==================== ==================
operator             semantics
==================== ==================
\\circ               forward composition
\\odot               reverse composition
==================== ==================




Atomic Forms
++++++++++++

Grouping is provided by parenthesis:

.. code-block:: felix

   (a + b) / c


Conditional
-----------

The standard boolean conditional expression:

.. code-block:: felix

  if cond then texpr else fexpr endif

The `else` clause is mandatory, the `endif` can sometimes be dropped
if the end of the alternate expression is established by context.
The `cond` expression must be of type `bool` and `texpr` and `fexpr`
must be the same type.


Pattern Match
-------------

Pattern match expression:

.. code-block:: felix

  match mexpr with
  | pattern1 => expr1
  | pattern2 => expr2
  ...
  endmatch

The `endmatch` is mandatory. All the RHS expressions must have
the same type. All the patterns must also match the same type
as `mexpr`. If all the patterns fail to match the program
terminates with a match failure.



