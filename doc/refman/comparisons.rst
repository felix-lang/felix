Comparisons
+++++++++++


Silly test \(a \leq b\) or $a \leq b$:

.. code-block:: felix

   a \leq b

.. code-block:: xfelix

   a \leq b


Syntax
------

.. code-block:: felix

  x[scomparison_pri]:= x[>scomparison_pri] cmp x[>scomparison_pri] 
  x[scomparison_pri]:= x[>scomparison_pri] "not" cmp x[>scomparison_pri]

Description
-----------

Felix provides a very large set of comparison operators.
A collection of TeX operators are used for defined
comparisons along with the usual ascii art symbols.

In addition there is a large collection of TeX operators which
are not currently used and are available for overloading
by the user. 

Equivalences
============

Syntax
------

.. code-block:: felix

  cmp := "==" 
  cmp := "!="
  cmp := "\ne"
  cmp := "\neq"

Semantics
---------

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

Description
-----------

An equivalence relation is a reflexive, symmetric, transitive comparison.
The prototypical equivalence relation is equality. Felix uses C's `==` operator
for equality and supplies the negated forms `!=`, `\ne` and `\neq` as as well.

Generic Equality
----------------

Felix provides a generic function `_eq` which generates an equality relation
for non-array tuples and records, and delegates to `==` for arrays, sums,
pointers, polymorphic variants and nominal types.

It is based on the bound value type and generates
and unbound expanded comparison which is subsequently bound, therefore
it depends on the names `_eq` and `==`.

The generated function is given the bound name `__eq`. The compiler
checks for this function and uses it if already defined, thereby avoiding
duplicate definitions.

[NOTE: It should work for unions and polymorphic variants too,
but is not implemented]

.. code-block:: felix

   println$ _eq ((a=1, b=2) , (a=1, b=2));


Partial Orders
==============

Syntax
------

.. code-block:: felix

  cmp := "\subset"
  cmp := "\supset" 
  cmp := "\subseteq" 
  cmp := "\subseteqq"
  cmp := "\supseteq" 
  cmp := "\supseteqq"

  cmp := "\nsubseteq"
  cmp := "\nsubseteqq"
  cmp := "\nsupseteq"
  cmp := "\nsupseteqq"

  cmp := "\subsetneq"
  cmp := "\subsetneqq"
  cmp := "\supsetneq"
  cmp := "\supsetneqq"

Semantics
---------

.. code-block:: felix

  class Pord[t]{
    inherit Eq[t];
    virtual fun \subset: t * t -> bool;
    virtual fun \supset(x:t,y:t):bool =>y \subset x;
    virtual fun \subseteq(x:t,y:t):bool => x \subset y or x == y;
    virtual fun \supseteq(x:t,y:t):bool => x \supset y or x == y;

    fun \subseteqq(x:t,y:t):bool => x \subseteq y;
    fun \supseteqq(x:t,y:t):bool => x \supseteq y;

    fun \nsubseteq(x:t,y:t):bool => not (x \subseteq y);
    fun \nsupseteq(x:t,y:t):bool => not (x \supseteq y);
    fun \nsubseteqq(x:t,y:t):bool => not (x \subseteq y);
    fun \nsupseteqq(x:t,y:t):bool => not (x \supseteq y);

    fun \supsetneq(x:t,y:t):bool => x \supset y;
    fun \supsetneqq(x:t,y:t):bool => x \supset y;
    fun \supsetneq(x:t,y:t):bool => x \supset y;
    fun \supsetneqq(x:t,y:t):bool => x \supset y;

    axiom trans(x:t, y:t, z:t): \subset(x,y) and \subset(y,z) implies \subset(x,z);
    axiom antisym(x:t, y:t): \subset(x,y) or \subset(y,x) or x == y;
    axiom reflex(x:t, y:t): \subseteq(x,y) and \subseteq(y,x) implies x == y;
  }

Description
-----------

An improper (non-strict) partial order is a reflexive, transitive, anti-symmetric
comparison. Proper (strict) partial orders are irreflexive. The prototypical
partial order is the subset relation. In type theory, subtype relations
are also partial orders.

Reference: https://en.wikipedia.org/wiki/Partially_ordered_set


Total Orders
============

Syntax
------

.. code-block:: felix

  cmp := "<" 

  cmp := "\lt"
  cmp := "\lneq" 
  cmp := "\lneqq" 

  cmp := "<=" 
  cmp := "\le"
  cmp := "\leq"
  cmp := "\leqq" 

  cmp := ">"
  cmp := "\gt"
  cmp := "\gneq" 
  cmp := "\gneqq" 

  cmp := ">=" 
  cmp := "\ge" 
  cmp := "\geq"
  cmp := "\geqq"

  cmp := "\nless"
  cmp := "\nleq" 
  cmp := "\nleqq"
  cmp := "\ngtr"
  cmp := "\ngeq" 
  cmp := "\ngeqq"

Semantics
---------

.. code-block:: felix

  class Tord[t]{
    inherit Eq[t];
    // defined in terms of <, argument order swap, and boolean negation

    // less
    virtual fun < : t * t -> bool;
    fun lt (x:t,y:t): bool=> x < y;
    fun \lt (x:t,y:t): bool=> x < y;
    fun \lneq (x:t,y:t): bool=> x < y;
    fun \lneqq (x:t,y:t): bool=> x < y;


    axiom trans(x:t, y:t, z:t): x < y and y < z implies x < z;
    axiom antisym(x:t, y:t): x < y or y < x or x == y;
    axiom reflex(x:t, y:t): x < y and y <= x implies x == y;
    axiom totality(x:t, y:t): x <= y or y <= x;


    // greater
    fun >(x:t,y:t):bool => y < x;
    fun gt(x:t,y:t):bool => y < x;
    fun \gt(x:t,y:t):bool => y < x;
    fun \gneq(x:t,y:t):bool => y < x;
    fun \gneqq(x:t,y:t):bool => y < x;

    // less equal
    fun <= (x:t,y:t):bool => not (y < x);
    fun le (x:t,y:t):bool => not (y < x);
    fun \le (x:t,y:t):bool => not (y < x);
    fun \leq (x:t,y:t):bool => not (y < x);
    fun \leqq (x:t,y:t):bool => not (y < x);
    fun \leqslant (x:t,y:t):bool => not (y < x);


    // greater equal
    fun >= (x:t,y:t):bool => not (x < y);
    fun ge (x:t,y:t):bool => not (x < y);
    fun \ge (x:t,y:t):bool => not (x < y);
    fun \geq (x:t,y:t):bool => not (x < y);
    fun \geqq (x:t,y:t):bool => not (x < y);
    fun \geqslant (x:t,y:t):bool => not (x < y);

    // negated, strike-through
    fun \ngtr (x:t,y:t):bool => not (x < y);
    fun \nless (x:t,y:t):bool => not (x < y);

    fun \ngeq (x:t,y:t):bool => x < y;
    fun \ngeqq (x:t,y:t):bool => x < y;
    fun \ngeqslant (x:t,y:t):bool => x < y;

    fun \nleq (x:t,y:t):bool => not (x <= y);
    fun \nleqq (x:t,y:t):bool => not (x <= y);
    fun \nleqslant (x:t,y:t):bool => not (x <= y);
    

    // maxima and minima
    fun max(x:t,y:t):t=> if x < y then y else x endif;
    fun \vee(x:t,y:t) => max (x,y);

    fun min(x:t,y:t):t => if x < y then x else y endif;
    fun \wedge(x:t,y:t):t => min (x,y);

  }

Description
-----------

An improper (non-strict) total, or linear order, is an anti-symmtric, transitive
relation with the connex property.

Reference: https://en.wikipedia.org/wiki/Total_order

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


