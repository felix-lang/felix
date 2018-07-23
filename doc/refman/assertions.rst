Assertions
==========

 
Assert
------

.. code-block:: felix

  assertion_stmt := "assert" sexpr ";"

The usual assert statement.
Abort the program if the argument expression evaluates to false
when control flows through the assert statement.
Cannot be switched off!

Axiom
-----

.. code-block:: felix

  assertion_stmt  := "axiom" sdeclname sfun_arg ":" sexpr ";"

Define an axiom with a general predicate.
An axiom is a function which is true for all arguments.
Axioms are core assertions about invariants which
can be used to specify semantics and form the basis
of reasoning about semantics which goes beyond
structure.

.. code-block:: felix

  assertion_stmt  := "axiom" sdeclname sfun_arg ":" sexpr "=" sexpr ";"

A variant of an axiom which expresses the semantic
equality of two expressions. Do not confuse this
with an expresion containing run time equality (==).
Semantic equality means that one expression could be
replaced by the other without any observable difference
in behaviour in any program, this can be asserted even
if the type does not provide an equality operator (==).

Lemma
-----

.. code-block:: felix

  assertion_stmt  := "lemma" sdeclname sfun_arg ":" sexpr ";"
  assertion_stmt  := "lemma" sdeclname sfun_arg ":" sexpr "=" sexpr ";"

A lemma is a proposition which it is expected could
be proved by a good automatic theorem prover,
given the axioms.

Theorem
-------

.. code-block:: felix

  assertion_stmt  := "theorem" sdeclname sfun_arg ":" sexpr proof? ";"
  assertion_stmt  := "theorem" sdeclname sfun_arg ":" sexpr "=" sexpr proof? ";"

A theorem is a proposition which it is expected could
NOT be proved by a good automatic theorem prover,
given the axioms.  In the future, we might like to
provide a "proof sketch" which a suitable tool could
fill in. For the present, you can give a proof as
a plain text in a string as a hint to the reader.

Reduction
---------

.. code-block:: felix

  assertion_stmt  := "reduce" sname "|"? sreductions ";"
    private sreduce_args := "(" stypeparameter_comma_list ")"
    private sreduction := stvarlist sreduce_args ":" sexpr "=>" sexpr
    private sreductions := sreduction 
    private sreductions := sreduction "|" sreductions


A reduction is a special kind of proposition of equational
form which also directs the compiler to actually replace
the LHS expression with the RHS expression when found.

Reductions allow powerful high level optimisations,
such as eliminating two successive list reversals.

The client must take great care that reductions don't
lead to infinite loops. Confluence isn't required but
is probably desirable.

Reductions should be used sparingly because searching
for patterns to reduce is applied to every sub-expression
of every expression in the whole program, repeatedly
after any reduction is applied, and this whole process
is done at several different places in the program,
to try to effect the reductions. Particularly both
before and after inlining, since that can destroy
or create candidate patterns.

