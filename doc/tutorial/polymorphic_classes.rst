Polymorphic Classes
===================

Polymorphic classes can be used to systematically introduce 
notation systems representing algebras.

For example let us start with a sketch of the library
class `Eq` which introduces equivalence relations:

.. code-block:: felix

  class Eq[t] {
    virtual fun == : t * t -> bool;
    virtual fun != (x:t,y:t):bool => not (x == y);

    axiom reflex(x:t): x == x;
    axiom sym(x:t, y:t): (x == y) == (y == x);
    axiom trans(x:t, y:t, z:t): x == y and y == z implies x == z;

    fun eq(x:t, y:t)=> x == y;
    fun ne(x:t, y:t)=> x != y;
  }


The class has a single type parameter `t`.
Two `virtual` functions are introduced first, `==` for equivalence,
and `!=` for inequivalence. 

The type of the `==` function is given but it is not defined.
We only have the interface.

The inequivalence is defined as the negation of equivalence.

Next we have three axioms which specify the required semantics.

The first axiom, `reflexivity`, says a value is equivalent to itself.

The second axiom, `symmetry`, says a if x is equivalent to y, then
y is equivalent to x. 

The third axiom, `transitivity` says if x is equivalent to y, and
y is equivalent to z, then z is equivalent to z too.

In this case these three laws are a complete specification, and,
the laws are independent, in that one cannot be deduced from another.
These two properties mean that our rules are in fact mathematical
axioms. 

Felix does not require `axioms` actually be axioms. However, simple
rules which can be derived from the stated axioms can be specified by
`lemma`. The idea is that lemmas can be so eaily proven from the axioms
that an automatic theorem prover can do it, without any further assistance.

You can also a `theorem` which is a rule that can be proven from
the axioms, but where the proof requires a human, or a human
with a proof assistant, to establish its correctness.

Finally in our class, we have two named function defined in terms
of the virtual functions. Notice these functions are not virtual.


