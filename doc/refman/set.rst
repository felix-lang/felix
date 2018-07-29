Sets
====

Sets provide an alternate representation of logical operators.

Syntax
------

.. code-block:: felix

  syntax setexpr
  {
    cmp := "in" 
    cmp := "\in"
    cmp := "\notin"
    cmp := "\owns" ; 

    x[ssetunion_pri] := x[ssetunion_pri] "\cup" x[>ssetunion_pri]
    x[ssetintersection_pri] := x[ssetintersection_pri] "\cap" x[>ssetintersection_pri] 
  }

Semantics
---------

.. code-block:: felix

  class Set[c,t] {
    fun mem (elt:t, container:c):bool => elt \in container;
    virtual fun \in : t * c-> bool;
    fun \owns (container:c, elt:t) => elt \in container;
    fun \ni (container:c, elt:t) => elt \in container;
    fun \notin (elt:t, container:c) => not (elt \in container);

    fun \cup[c2 with Set[c2,t]] 
      (x:c, y:c2) => 
      { e : t | e \in x or e \in y }
    ;

    fun \cap[c2 with Set[c2,t]] 
      (x:c, y:c2) => 
      { e : t | e \in x and e \in y }
    ;

    fun \setminus[c2 with Set[c2,t]] 
      (x:c, y:c2) => 
      { e : t | e \in x and e \notin y }
    ;
  }

A `set` is any type with a membership predicate :math:`\in``
spelled `\\in`. You can also use function `mem`. The parser
also maps `in` to operator `\\in`.

We also provide a reversed form :math::`\owns` spelled `\\owns`,
and negated forms :math:`\ni` spelled `\ni` or `\notin`.

Three combinators are provided as well, :math:`\cap` spelled `\\cap`
provides intersection, :math:`\cup` spelled `\\cup` provides
the usual set union, and :math:`\setminus` spelled `\setminus`
the asymmetic set difference or subtraction.

Note that sets are not necessarily finite.
