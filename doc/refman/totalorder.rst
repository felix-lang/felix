Total Order
===========

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

Syntax
------

.. code-block:: felix

  syntax tordcmpexpr
  {
    cmp := "<" =># "(nos _1)"; 

    cmp := "\lt" =># '(nos _1)'; 
    cmp := "\lneq" =># '(nos _1)'; 
    cmp := "\lneqq" =># '(nos _1)'; 

    cmp := "<=" =># "(nos _1)"; 
    cmp := "\le" =># '(nos _1)'; 
    cmp := "\leq" =># '(nos _1)'; 
    cmp := "\leqq" =># '(nos _1)'; 

    cmp := ">" =># "(nos _1)"; 
    cmp := "\gt" =># '(nos _1)'; 
    cmp := "\gneq" =># '(nos _1)'; 
    cmp := "\gneqq" =># '(nos _1)'; 

    cmp := ">=" =># "(nos _1)"; 
    cmp := "\ge" =># '(nos _1)'; 
    cmp := "\geq" =># '(nos _1)'; 
    cmp := "\geqq" =># '(nos _1)'; 

    cmp := "\nless" =># '(nos _1)'; 
    cmp := "\nleq" =># '(nos _1)'; 
    cmp := "\nleqq" =># '(nos _1)'; 
    cmp := "\ngtr" =># '(nos _1)'; 
    cmp := "\ngeq" =># '(nos _1)'; 
    cmp := "\ngeqq" =># '(nos _1)'; 

    bin := "\vee" =># '(nos _1)'; 
    bin := "\wedge" =># '(nos _1)'; 
  }


