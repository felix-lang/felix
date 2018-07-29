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


