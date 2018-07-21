Arithetic Operators
===================

Addition
++++++++

Syntax
------

.. code-block:: felix

  //$ Addition: left non-associative.
  x[ssum_pri] := x[>ssum_pri] ("+" x[>ssum_pri])+ 

  //$ Subtraction: left associative.
  x[ssubtraction_pri] := x[ssubtraction_pri] "-" x[sproduct_pri] 

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


Multiplication
++++++++++++++

.. code-block:: felix

  //$ multiplication: right associative
  x[sproduct_pri] := x[>sproduct_pri] "\otimes" x[sproduct_pri] 

  //$ multiplication: non-associative.
  x[sproduct_pri] := x[>sproduct_pri] ("*" x[>sproduct_pri])+ 

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

  class FloatRing[t] {
    inherit FloatAddgrp[t];
    inherit FloatMultSemi1[t];
  }

  class Ring[t] {
    inherit Addgrp[t];
    inherit MultSemi1[t];
    axiom distrib (x:t,y:t,z:t): x * ( y + z) == x * y + x * z;
  }

Dvision
+++++++

Syntax
------

.. code-block:: felix

  //$ division: right associative low precedence fraction form
  x[stuple_pri] := x[>stuple_pri] "\over" x[>stuple_pri] 

  //$ division: left associative.
  x[s_term_pri] := x[s_term_pri] "/" x[>s_term_pri] 

  //$ remainder: left associative.
  x[s_term_pri] := x[s_term_pri] "%" x[>s_term_pri]

  //$ remainder: left associative.
  x[s_term_pri] := x[s_term_pri] "\bmod" x[>s_term_pri]

Semantics
---------

.. code-block:: felix

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

  class Dring[t] {
    inherit Ring[t];
    inherit FloatDring[t];
  }

Repeated Sum Type
+++++++++++++++++

.. code-block:: felix

  // repeated sum type, eg 4 *+ int == int + int + int + int
  // right associative:  2 *+ 3 *+ int is approx 6 *+ int
  x[sproduct_pri] := x[>sproduct_pri] "*+" x[sproduct_pri]


Prefix Forms
++++++++++++

.. code-block:: felix

  //$ Prefix exclaim.
  x[sprefixed_pri] := "!" x[spower_pri]

  //$ Prefix plus.
  x[sprefixed_pri] := "+" x[spower_pri] 

  //$ Prefix negation.
  x[sprefixed_pri] := "-" x[spower_pri]

  //$ Prefix complement.
  x[sprefixed_pri] := "~" x[spower_pri]

Power forms
++++++++++++

.. code-block:: felix

  //$ Fortran power.
  x[spower_pri] := x[ssuperscript_pri] "**" x[sprefixed_pri]
  x[spower_pri] := x[ssuperscript_pri] "<**>" x[sprefixed_pri] 

Superscipts
+++++++++++

.. code-block:: felix

  //$ Superscript, exponential.
  x[ssuperscript_pri] := x[ssuperscript_pri] "^" x[srefr_pri] 

  //$ composition
  x[ssuperscript_pri] := x[ssuperscript_pri] "\circ" x[>ssuperscript_pri]
  x[ssuperscript_pri] := x[ssuperscript_pri] "\cdot" x[>ssuperscript_pri] 


