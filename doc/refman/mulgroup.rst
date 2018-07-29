Multiplicative SemiGroup with Unit
==================================

Syntax
------

.. code-block:: felix

  syntax mulexpr
  {
    //$ multiplication: non-associative.
    x[sproduct_pri] := x[>sproduct_pri] ("*" x[>sproduct_pri])+ =># 
      "(chain 'ast_product _1 _2)" note "mul";
  }


Semantics
---------

.. code-block:: felix

  class FloatMultSemi1[t] {
    inherit Eq[t];
    proc muleq(px:&t, y:t) { *= (px,y); }
    fun mul(x:t, y:t) => x * y;
    fun sqr(x:t) => x * x;
    fun cube(x:t) => x * x * x;
    virtual fun one: unit -> t;
    virtual fun * : t * t -> t;
    virtual proc *= (px:&t, y:t) { px <- *px * y; }
    //reduce id (x:t): x*one() => x;
    //reduce id (x:t): one()*x => x;
  }

  class MultSemi1[t] {
    inherit FloatMultSemi1[t];
    axiom assoc (x:t,y:t,z:t): (x * y) * z == x * (y * z);
    //reduce cancel (x:t,y:t,z:t): x * z ==  y * z => x == y;
  }


Description
------------

A multiplicative semigroup with unit is an approximate
multiplicative semigroup with unit and associativity
and satisfies the cancellation law.

