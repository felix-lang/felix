Logical Operations
==================

Syntax
------

.. code-block:: felix

  //$ Boolean false.
  satom := "false" =># "'(ast_typed_case  0 2)";

  //$ Boolean true.
  satom := "true" =># "'(ast_typed_case  1 2)";

  //$ Logical implication.
  x[simplies_condition_pri] := x[>simplies_condition_pri] "implies" x[>simplies_condition_pri] 

  //$ Logical disjunction (or).
  x[sor_condition_pri] := x[>sor_condition_pri] ( "or" x[>sor_condition_pri])+ 

  //$ Logical conjunction (and).
  x[sand_condition_pri] := x[>sand_condition_pri] ( "and" x[>sand_condition_pri])+ 

  //$ Logical negation (not).
  x[snot_condition_pri] := "not" x[snot_condition_pri]  


  // tex logic operators
  x[stex_implies_condition_pri] := x[>stex_implies_condition_pri]  "\implies" x[>stex_implies_condition_pri] 

  x[stex_or_condition_pri] := x[>stex_or_condition_pri] ( "\lor" x[>stex_or_condition_pri])+ 

  x[stex_and_condition_pri] := x[>stex_and_condition_pri] ( "\land" x[>stex_and_condition_pri])+ 

  x[stex_not_condition_pri] := "\lnot" x[stex_not_condition_pri]


  bin := "\iff" =># '(nos _1)'; // NOT IMPLEMENTED FIXME
  bin := "\impliedby" =># '(nos _1)'; // NOT IMPLEMENTED FIXME

  //$ Conditional expression.
  satom := sconditional "endif" =># "_1";

  //$ Conditional expression (prefix).
  sconditional := "if" sexpr "then" sexpr selse_part 

      selif := "elif" sexpr "then" sexpr 

      selifs := selif 
      selifs := selifs selif 

      selse_part:= "else" sexpr 
      selse_part:= selifs "else" sexpr 
}

Semantics
---------

.. code-block:: felix

  //$ Bitwise operators.
  class Bits[t] {
    virtual fun \^ : t * t -> t = "(?1)($1^$2)";
    virtual fun \| : t * t -> t = "$1|$2";
    virtual fun \& : t * t -> t = "$1&$2";
    virtual fun ~: t -> t = "(?1)(~$1)";
    virtual proc ^= : &t * t = "*$1^=$2;";
    virtual proc |= : &t * t = "*$1|=$2;";
    virtual proc &= : &t * t = "*$1&=$2;";

    fun bxor(x:t,y:t)=> x \^ y;
    fun bor(x:t,y:t)=> x \| y;
    fun band(x:t,y:t)=> x \& y;
    fun bnot(x:t)=> ~ x;

  }


Description
-----------

Felix provides two boolean types: :code:`bool` and :code:`cbool`
with constants :code:`true` and :code:`false` and
:code:`ctrue` and :code:`cfalse` respectively and
the following basic operations:

========== ======== =========
Operator   Function Semantics
========== ======== =========
or         lor      disjunction
orelse     orelse   lazy disjunction
\          nor      negated disjunction
and        land     conjunction
andthen    andthen  lazy conjunction
\          nand     negated conjunction
not        not      negation
implies    implies  implication
========== ======== =========

The lazy forms require a function of type
:code:`1->bool` and :code:`1->cbool` respectively
for their second argument, which is evaluated only if the 
first argument is does not determine the final value.

The type bool is an alias for the sum type 2,
which is a compact linear type and will cost 64bits of store.

The type cbool is a binding to C++ bool, and is typically
one byte. Functionally these types are equivalent however
pointers to cbool are sometimes required for C++ compatibility.

