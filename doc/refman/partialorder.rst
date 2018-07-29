Partial Order
=============

Syntax
------

.. code-block:: felix

  syntax pordcmpexpr
  {
    cmp := "\subset" =># '(nos _1)'; 
    cmp := "\supset" =># '(nos _1)'; 
    cmp := "\subseteq" =># '(nos _1)'; 
    cmp := "\subseteqq" =># '(nos _1)'; 
    cmp := "\supseteq" =># '(nos _1)'; 
    cmp := "\supseteqq" =># '(nos _1)'; 

    cmp := "\nsubseteq" =># '(nos _1)'; 
    cmp := "\nsubseteqq" =># '(nos _1)'; 
    cmp := "\nsupseteq" =># '(nos _1)'; 
    cmp := "\nsupseteqq" =># '(nos _1)'; 

    cmp := "\subsetneq" =># '(nos _1)'; 
    cmp := "\subsetneqq" =># '(nos _1)'; 
    cmp := "\supsetneq" =># '(nos _1)'; 
    cmp := "\supsetneqq" =># '(nos _1)'; 
  }

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

