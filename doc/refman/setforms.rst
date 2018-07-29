Sets
====

Sets provide an alternate representation of logical operators.

Set Class
---------

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

Syntax
------

.. code-block:: felix

  syntax setexpr
  {
    cmp := "in" 
    cmp := "\in"
    cmp := "\notin"
    cmp := "\owns" ; 

    x[ssetunion_pri] := x[ssetunion_pri] "\cup" x[>ssetunion_pri] =># "(Infix)" note "setunion";
    x[ssetintersection_pri] := x[ssetintersection_pri] "\cap" x[>ssetintersection_pri] =># "(Infix)" 
      note "setintersection";
  }

Set Forms
=========

A `set_form` is a record type with a single 
member `has_elt` which returns true if it's argument
is intended as a member of some particular set.

We construe a set_form as a Set by providing an
instance.

A set_form is basically just the membership predicate remodelled
as a noun by encapsulating the predicate in a closure and
thereby abstracting it.

.. code-block:: felix

  interface set_form[T] { has_elt: T -> bool; }

  instance[T] Set[set_form[T], T] {
    fun \in (elt:T, s:set_form[T]) => s.has_elt elt;
  }
  open[T] Set[set_form[T],T];

We provide an inverse image:

.. code-block:: felix

  fun invimg[t,c2,t2 with Set[c2,t2]] 
    (f:t->t2, x:c2) : set_form[t] =>
    { e : t | (f e) \in x }
  ;

Cartesian Product of set_forms.
-------------------------------

This uses some advanced instantiation technology
to allow you to define the cartesian product of a
sequence of sets using the infix TeX operator :math:`\otimes`
which is spelled `\otimes`. There's also a left associative
binary operator :math:`\times` spelled `\times`.

Operators
---------

.. code-block:: felix

  fun \times[U,V] (x:set_form[U],y:set_form[V]) => 
    { u,v : U * V | u \in x and v \in y }
  ;

  fun \otimes[U,V] (x:set_form[U],y:set_form[V]) => 
    { u,v : U * V | u \in x and v \in y }
  ;

  fun \otimes[U,V,W] (head:set_form[U], tail:set_form[V*W]) =>
    { u,v,w : U * V * W | u \in head and (v,w) \in tail }
  ;

  fun \otimes[NH,OH,OT] (head:set_form[NH], tail:set_form[OH**OT]) =>
    { h,,(oh,,ot) : NH ** (OH ** OT) | h \in head and (oh,,ot) \in tail }
  ;


Example:

.. code-block:: felix

  var p = \{ x,y: int * int | x == y \};
  println$ (1,1) in p;


Containers
===========

Roughly, a Container is a finite Set.
It is a derived type specified in the library
with a type class:

.. code-block:: felix

  class Container [c,v]
  {
    inherit Set[c,v];
    virtual fun len: c -> size;
    fun \Vert (x:c) => len x;
    virtual fun empty(x: c): bool => len x == size(0);
  }


The :math:`\Vert` operator, spelled `\Vert` is an alternative
name for `len`.


