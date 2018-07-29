Equivalence Relation
====================

Syntax
------

.. code-block:: felix

  syntax cmpexpr
  {
    x[scomparison_pri]:= x[>scomparison_pri] cmp x[>scomparison_pri] =># "`(ast_apply ,_sr (,_2 (,_1 ,_3)))";
    x[scomparison_pri]:= x[>scomparison_pri] "not" cmp x[>scomparison_pri] =># "`(ast_not ,_sr (ast_apply ,_sr (,_3 (,_1 ,_4))))";
    cmp := "==" =># "(nos _1)"; 
    cmp := "!=" =># "(nos _1)"; 
    cmp := "\ne" =># '(nos _1)'; 
    cmp := "\neq" =># '(nos _1)'; 
  }

Semantics
---------

.. code-block:: felix

  class Eq[t] {
    virtual fun == : t * t -> bool;
    virtual fun != (x:t,y:t):bool => not (x == y);

    axiom reflex(x:t): x == x;
    axiom sym(x:t, y:t): (x == y) == (y == x);
    axiom trans(x:t, y:t, z:t): x == y and y == z implies x == z;

    fun eq(x:t, y:t)=> x == y;
    fun ne(x:t, y:t)=> x != y;
    fun \ne(x:t, y:t)=> x != y;
    fun \neq(x:t, y:t)=> x != y;
  }


