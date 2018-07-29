Additive Groups
===============

Syntax
------

.. code-block:: felix

  syntax addexpr
  {
    //$ Addition: left non-associative.
    x[ssum_pri] := x[>ssum_pri] ("+" x[>ssum_pri])+ =># "(chain 'ast_sum _1 _2)" note "add";

    //$ Subtraction: left associative.
    x[ssubtraction_pri] := x[ssubtraction_pri] "-" x[sproduct_pri] =># "(Infix)";
  }


Semantics
---------
.. code-block:: felix

  class FloatAddgrp[t] {
    inherit Eq[t];
    virtual fun zero: unit -> t;
    virtual fun + : t * t -> t;
    virtual fun neg : t -> t;
    virtual fun prefix_plus : t -> t = "$1";
    virtual fun - (x:t,y:t):t => x + -y;
    virtual proc += (px:&t,y:t) { px <- *px + y; }
    virtual proc -= (px:&t,y:t) { px <- *px - y; }

  /*
    reduce id (x:t): x+zero() => x;
    reduce id (x:t): zero()+x => x;
    reduce inv(x:t): x - x => zero();
    reduce inv(x:t): - (-x) => x;
  */
    axiom sym (x:t,y:t): x+y == y+x;

    fun add(x:t,y:t)=> x + y;
    fun plus(x:t)=> +x;
    fun sub(x:t,y:t)=> x - y;
    proc pluseq(px:&t, y:t) {  += (px,y); }
    proc  minuseq(px:&t, y:t) { -= (px,y); }
  }

  class Addgrp[t] {
    inherit FloatAddgrp[t];
    axiom assoc (x:t,y:t,z:t): (x + y) + z == x + (y + z);
    //reduce inv(x:t,y:t): x + y - y => x;
  }



