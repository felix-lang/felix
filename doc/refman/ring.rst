Rings
=====

Syntax
------

.. code-block:: felix

  syntax divexpr
  {
    //$ division: right associative low precedence fraction form
    x[stuple_pri] := x[>stuple_pri] "\over" x[>stuple_pri]

    //$ division: left associative.
    x[s_term_pri] := x[s_term_pri] "/" x[>s_term_pri]

    //$ remainder: left associative.
    x[s_term_pri] := x[s_term_pri] "%" x[>s_term_pri]

    //$ remainder: left associative.
    x[s_term_pri] := x[s_term_pri] "\bmod" x[>s_term_pri]
  }


Semantics
---------

.. code-block:: felix

  // Approximate Ring with Unit
  class FloatRing[t] {
    inherit FloatAddgrp[t];
    inherit FloatMultSemi1[t];
  }

  // Ring with Unit
  class Ring[t] {
    inherit Addgrp[t];
    inherit MultSemi1[t];
    axiom distrib (x:t,y:t,z:t): x * ( y + z) == x * y + x * z;
  }

  // Approximate Division Ring
  class FloatDring[t] {
    inherit FloatRing[t];
    virtual fun / : t * t -> t; // pre t != 0
    fun \over (x:t,y:t) => x / y;

    virtual proc /= : &t * t;
    virtual fun % : t * t -> t;
    virtual proc %= : &t * t;

    fun div(x:t, y:t) => x / y;
    fun mod(x:t, y:t) => x % y;
    fun \bmod(x:t, y:t) => x % y;
    fun recip (x:t) => #one / x;

    proc diveq(px:&t, y:t) { /= (px,y); }
    proc modeq(px:&t, y:t) { %= (px,y); }
  }

  // Division Ring
  class Dring[t] {
    inherit Ring[t];
    inherit FloatDring[t];
  }

Description
-----------

Approximate Unit Ring.
  An approximate ring is a set which has addition and
  multiplication satisfying the rules for approximate
  additive group and multiplicative semigroup with unit respectively.

Ring.
  A ring is a type which is a both an additive group and
  multiplicative semigroup with unit, and which in
  addition satisfies the distributive law.

Approximate Division Ring.
  An approximate division ring is an approximate ring with unit
  with a division operator.

Division Ring.
   An associative approximate division ring.

