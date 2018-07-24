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

Precedence Indices
------------------

.. code-block:: felix

  priority
    let_pri <
    slambda_pri <
    spipe_apply_pri <
    sdollar_apply_pri <
    stuple_cons_pri <
    stuple_pri <

    // logic
    simplies_condition_pri <
    sor_condition_pri <
    sand_condition_pri <
    snot_condition_pri <
    stex_implies_condition_pri <
    stex_or_condition_pri <
    stex_and_condition_pri <
    stex_not_condition_pri <

    scomparison_pri <
    sas_expr_pri <

    // sets
    ssetunion_pri <
    ssetintersection_pri <

    sarrow_pri <
    scase_literal_pri <

    // bitwise operators
    sbor_pri <
    sbxor_pri <
    sband_pri <
    sshift_pri <

    // numeric operators
    ssum_pri <
    ssubtraction_pri <
    sproduct_pri <
    s_term_pri <
    sprefixed_pri <
    spower_pri <
    ssuperscript_pri <

    srefr_pri <
    scoercion_pri <
    sapplication_pri <
    sfactor_pri <
    srcompose_pri <
    sthename_pri <
    satomic_pri
  ;



Operator Whitespace
+++++++++++++++++++

.. code-block:: felix

  //$ Optimisation hint: likely.
  //$ Use in conditionals, e.g. if likely(x) do ...
  x[sapplication_pri] := "likely" x[>sapplication_pri]

  //$ Optimisation hint: unlikely.
  //$ Use in conditionals, e.g. if unlikely(x) do ...
  x[sapplication_pri] := "unlikely" x[>sapplication_pri] 

Coercions
+++++++++

.. code-block:: felix

  //$ Suffixed coercion.
  x[scoercion_pri] := x[scoercion_pri] ":>>" x[>scoercion_pri]

Reverse Composition
+++++++++++++++++++

.. code-block:: felix

  //$ Reverse composition
  x[srcompose_pri] := x[srcompose_pri] "\odot" x[>srcompose_pri] 

Addressing
++++++++++

.. code-block:: felix

  //$ macro expansion freezer.
  x[sthename_pri] := "noexpand" squalified_name 

  //$ pattern variable.
  x[sthename_pri] := "?" sname 

  //$ Template replacement index.
  x[sthename_pri] := "?" sinteger

  x[sthename_pri] := squalified_name


Composition Sumary
++++++++++++++++++

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


