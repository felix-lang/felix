Precedence Indices
==================

The grammar provides a standard sequence of precedence indices.
Grammar rules for expressions generally look like this:

.. code-block:: felix

  x[s_term_pri] := x[s_term_pri] "/" x[>s_term_pri] =># '(Infix)';

The name `x` is used for expressions, the precedence index is a special
modifier on the LHS of the production. In the middle part, `x[s_term_pri]`
means any expression with the same precedence or higher, whilst
`x[>s_term_pri]` means any expression with a strictly higher precedence.
Thus, this means the division operator `/` is left associative.
  
On the other hand:

.. code-block:: felix
 
  x[sdollar_apply_pri] := x[>sdollar_apply_pri] "$" x[sdollar_apply_pri] =># 
    "`(ast_apply ,_sr (,_1 ,_3))"
  ;

which means the Haskell `$` application operator is right associative.


Here is the precedence specification from the grammar:

.. code-block:: felix

  priority
    let_pri <
    slambda_pri <

    // low precedence applications
    spipe_apply_pri <
    sdollar_apply_pri <

    // tuples
    stuple_cons_pri <
    stuple_pri <

    // basic logic
    simplies_condition_pri <
    sor_condition_pri <
    sand_condition_pri <
    snot_condition_pri <


    // TeX symbol logic
    stex_implies_condition_pri <
    stex_or_condition_pri <
    stex_and_condition_pri <
    stex_not_condition_pri <

    scomparison_pri <
    sas_expr_pri <

    // set operators
    ssetunion_pri <
    ssetintersection_pri <

    // arrow types
    sarrow_pri <

    // constructor forms
    scase_literal_pri <

    // bitwise operators
    sbor_pri <
    sbxor_pri <
    sband_pri <
    sshift_pri <

    // numeric operators
    ssum_pri <
    ssubtraction_pri <
    sproduct_pri <
    s_term_pri <
    sprefixed_pri <
    spower_pri <
    ssuperscript_pri <

    // addression operations
    srefr_pri <
    scoercion_pri <

    // high precedence applications
    sapplication_pri <

    sfactor_pri <
    srcompose_pri <
    sthename_pri <

    // atomic forms
    satomic_pri
  ;


