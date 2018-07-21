Expressions
===========

Expressions in generally used to perform calculations and construct
values. Because Felix has a user defined grammar, there are many
expression forms which reduce to function applications. In turn,
since functions are not permitted side-effects, with some caveats
expression forms can be regarded as referentially transparent.

The two main caveats are generators and impurity.

When an expression contains a direct generator application,
it is lifted out of the expression: the application is replaced
by a variable which is initialised before the expression is evaluated.
After the lift and replacement, the remaining expression may be free
of effects. However, generators have the same type as functions,
so if the application is indirect, for example the application
of a closure, Felix doesn't know if it is a generator or function
and may or may not lift it out.

Some functions may depend on variables and indeed, and expression
can contain variables. Since the evaluation is side-effect free the
variable cannot change during the evaluation of the expression.
But it can change in a loop so that a subsequent evaluation
returns a different result. Of course the most trivial case
is when the expression is nothing more than a variable, such
as a loop control variable, in which case we'd be surprised
if the value didn't change!

Precedence Indices
------------------

.. code-block:: felix

  priority
    let_pri <
    slambda_pri <
    spipe_apply_pri <
    sdollar_apply_pri <
    stuple_cons_pri <
    stuple_pri <

    // logic
    simplies_condition_pri <
    sor_condition_pri <
    sand_condition_pri <
    snot_condition_pri <
    stex_implies_condition_pri <
    stex_or_condition_pri <
    stex_and_condition_pri <
    stex_not_condition_pri <

    scomparison_pri <
    sas_expr_pri <

    // sets
    ssetunion_pri <
    ssetintersection_pri <

    sarrow_pri <
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

    srefr_pri <
    scoercion_pri <
    sapplication_pri <
    sfactor_pri <
    srcompose_pri <
    sthename_pri <
    satomic_pri
  ;



Let forms
+++++++++

Syntax:

.. code-block:: felix

  x[let_pri] := "let" spattern "=" x[let_pri] "in" x[let_pri] 
  x[let_pri] := "let" spattern "=" x[let_pri] "in" x[let_pri] 
  x[let_pri] := "let" "fun" sdeclname fun_return_type "=" smatching+ "in" x[let_pri]
  x[let_pri] := "let" pattern_match 
  x[let_pri] := pattern_match 

A let form allows an expression to be factored:

.. code-block:: felix

  let p = expr1 in expr2

for example:

.. code-block:: felix

  let x2 = x * x in
  let y2 = y * y in
    sqrt (x2 + y2)

Another let form defines a local function:

.. code-block:: felix

  let fun sq(x:int) = x * x in 
    sqrt (sq x + sq y)

Dollar Forms
++++++++++++

.. code-block:: felix

  //$ Alternate conditional expression.
  x[sdollar_apply_pri] := x[stuple_pri] "unless" x[let_pri] "then" x[sdollar_apply_pri]
 
  //$ Low precedence right associative application.
  x[sdollar_apply_pri] := x[stuple_pri] "$" x[sdollar_apply_pri] 

Tuple Forms
+++++++++++

.. code-block:: felix

  //$ Tuple formation by cons: right associative.
  x[stuple_cons_pri] := x[>stuple_cons_pri] ",," x[stuple_cons_pri]

  //$ Tuple formation by append: left associative
  x[stuple_cons_pri] := x[stuple_cons_pri] "<,,>" x[>stuple_cons_pri] 

  //$ Tuple formation non-associative.
  x[stuple_pri] := x[>stuple_pri] ( "," x[>stuple_pri])+ 



Pipe Application
++++++++++++++++

.. code-block:: felix

  //$ Low precedence left associative reverse application.
  x[spipe_apply_pri] := x[spipe_apply_pri] "|>" x[stuple_pri] 


Infix Application
+++++++++++++++++

.. code-block:: felix

  //$ Haskell-ish style infix notation of functions   foo(x,y) => x `(foo) y
  x[stuple_pri]  := x[stuple_pri] "`(" sexpr ")" sexpr =># " `(ast_apply ,_sr ( ,_3 (,_1 ,_5)))";


Inline Variables
++++++++++++++++

.. code-block:: felix

  //$ Named temporary value.
  x[sas_expr_pri] := x[sas_expr_pri] "as" sname 


  //$ Named variable.
  x[sas_expr_pri] := x[sas_expr_pri] "as" "var" sname 


Slices
++++++

.. code-block:: felix

  x[sarrow_pri] := x[>sarrow_pri] ".." x[>sarrow_pri] 
  x[sarrow_pri] := x[>sarrow_pri] "..<" x[>sarrow_pri]
  x[sarrow_pri] := "..<" x[>sarrow_pri] 
  x[sarrow_pri] := ".." x[>sarrow_pri] 
  x[sarrow_pri] := x[>sarrow_pri] ".." 
  x[sarrow_pri] := ".." 
  x[sarrow_pri] := x[>sarrow_pri] ".+" x[>sarrow_pri] 

Arrows
------

.. code-block:: felix

  //$ Function type, right associative.
  x[sarrow_pri] := x[>sarrow_pri] "->" x[sarrow_pri] 
  x[sarrow_pri] := x[>sarrow_pri] "->" "[" sexpr "]" x[sarrow_pri] 

  //$ C function type, right associative.
  x[sarrow_pri] := x[>sarrow_pri] "-->" x[sarrow_pri] 

Case Literals
+++++++++++++

.. code-block:: felix

  //$ Case tag literal.
  x[scase_literal_pri] := "case" sinteger 
  x[scase_literal_pri] := "`" sinteger 

  //$ Case value.
  x[scase_literal_pri] := "case" sinteger "of" x[ssum_pri] 
  x[scase_literal_pri] := "`" sinteger "of" x[ssum_pri] 
  x[scase_literal_pri] := "`" sinteger ":" x[ssum_pri] 

  //$ Tuple projection function.
  x[scase_literal_pri] := "proj" sinteger "of" x[ssum_pri] 

  // coarray injection
  // (ainj (r:>>4) of (4 *+ int)) 42
  x[scase_literal_pri] := "ainj"  stypeexpr "of" x[ssum_pri] 


Unit Application
++++++++++++++++

.. code-block:: felix

  spv_name := "case" sname
  spv_name := "`" sname 

  //$ Variant value.
  x[sthename_pri] := "#" spv_name
  x[sapplication_pri] := spv_name  x[>sapplication_pri] 

Addition
++++++++

.. code-block:: felix

  //$ Addition: left non-associative.
  x[ssum_pri] := x[>ssum_pri] ("+" x[>ssum_pri])+ 

  //$ Subtraction: left associative.
  x[ssubtraction_pri] := x[ssubtraction_pri] "-" x[sproduct_pri] 

Multiplication
++++++++++++++

.. code-block:: felix

  //$ multiplication: right associative
  x[sproduct_pri] := x[>sproduct_pri] "\otimes" x[sproduct_pri] 

  //$ multiplication: non-associative.
  x[sproduct_pri] := x[>sproduct_pri] ("*" x[>sproduct_pri])+ 

Dvision
+++++++

.. code-block:: felix

  //$ division: right associative low precedence fraction form
  x[stuple_pri] := x[>stuple_pri] "\over" x[>stuple_pri] 

  //$ division: left associative.
  x[s_term_pri] := x[s_term_pri] "/" x[>s_term_pri] 

  //$ remainder: left associative.
  x[s_term_pri] := x[s_term_pri] "%" x[>s_term_pri]

  //$ remainder: left associative.
  x[s_term_pri] := x[s_term_pri] "\bmod" x[>s_term_pri]


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

Dereference
+++++++++++

.. code-block:: felix

  //$ C dereference.
  x[srefr_pri] := "*" x[srefr_pri]

  //$ Deref primitive.
  //x[srefr_pri] := "_deref" x[srefr_pri] 

  //$ Operator new.
  x[srefr_pri] := "new" x[srefr_pri]

Operator Whitespace
+++++++++++++++++++

.. code-block:: felix

  //$ Operator whitespace: application.
  x[sapplication_pri] := x[sapplication_pri] x[>sapplication_pri] 

  //$ Variant index.
  x[sapplication_pri] := "caseno" x[>sapplication_pri]
  x[sapplication_pri] := "casearg" x[>sapplication_pri] 

  //$ Optimisation hint: likely.
  //$ Use in conditionals, e.g. if likely(x) do ...
  x[sapplication_pri] := "likely" x[>sapplication_pri]

  //$ Optimisation hint: unlikely.
  //$ Use in conditionals, e.g. if unlikely(x) do ...
  x[sapplication_pri] := "unlikely" x[>sapplication_pri] 

Coercions
+++++++++

.. code-block:: felix

  //$ Suffixed coercion.
  x[scoercion_pri] := x[scoercion_pri] ":>>" x[>scoercion_pri]

Factors
+++++++

.. code-block:: felix

  //$ Reverse application.
  x[sfactor_pri] := x[sfactor_pri] "." x[>sfactor_pri]


  //$ Reverse application with dereference.
  //$ a *. b same as (*a) . b, like C  a -> b.
  x[sfactor_pri] := x[sfactor_pri] "*." x[>sfactor_pri]

  //$ a &. b is similar to &a . b for an array, but can be overloaded
  //$ for abstract arrays: like a + b in C. Returns pointer.
  // x[sfactor_pri] := x[sfactor_pri] "&." sthe_name
  x[sfactor_pri] := x[sfactor_pri] "&." x[>sfactor_pri]

Reverse Composition
+++++++++++++++++++

.. code-block:: felix

  //$ Reverse composition
  x[srcompose_pri] := x[srcompose_pri] "\odot" x[>srcompose_pri] =># "(Infix)";

Addressing
++++++++++

.. code-block:: felix

  //$ High precedence unit application. #f = f ().
  x[sthename_pri] := "#" x[sthename_pri] 

  //$ Felix pointer type and address of operator.
  x[sthename_pri] := "&" x[sthename_pri] 

  //$ Felix pointer type and address of operator.
  x[sthename_pri] := "_uniq" x[sthename_pri] 
  x[sthename_pri] := "_rref" x[sthename_pri]
  x[sthename_pri] := "&<" x[sthename_pri] 
  x[sthename_pri] := "_wref" x[sthename_pri]
  x[sthename_pri] := "&>" x[sthename_pri]


  //$ Felix address of operator.
  x[sthename_pri] := "label_address" sname 


  //$ C pointer type.
  x[sthename_pri] :=  "@" x[sthename_pri]

  //$ macro expansion freezer.
  x[sthename_pri] := "noexpand" squalified_name 

  //$ pattern variable.
  x[sthename_pri] := "?" sname 

  //$ Template replacement index.
  x[sthename_pri] := "?" sinteger

  x[sthename_pri] := squalified_name

Qualified Names
+++++++++++++++

.. code-block:: felix

  //$ Qualified name.
  sreally_qualified_name := squalified_name "::" ssimple_name_parts 

  squalified_name := sreally_qualified_name 

  squalified_name := ssimple_name_parts 
    

  ssimple_name_parts := sname 
  ssimple_name_parts := sname "[" "]"
  ssimple_name_parts := sname "[" sexpr "]" 

  //$ Suffixed name (to name functions).
  ssuffixed_name := squalified_name "of" x[sthename_pri] 
  


Comparisons
+++++++++++

.. code-block:: felix

  x[scomparison_pri]:= x[>scomparison_pri] cmp x[>scomparison_pri] =># "`(ast_apply ,_sr (,_2 (,_1 ,_3)))";
  x[scomparison_pri]:= x[>scomparison_pri] "not" cmp x[>scomparison_pri] =># "`(ast_not ,_sr (ast_apply ,_sr (,_3 (,_1 ,_4))))";

Equivalences
------------

.. code-block:: felix
  cmp := "==" =># "(nos _1)";
  cmp := "!=" =># "(nos _1)";
  cmp := "\ne" =># '(nos _1)';
  cmp := "\neq" =># '(nos _1)';

Partial Orders
--------------

.. code-block:: felix
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

Total Orders
------------

.. code-block:: felix
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

The usual comparison operators are available along with TeX identifiers:

==================== ==================
operator             numeric semantics
==================== ==================
==, \\eq              equality
!=, \\ne              inequality

<, \\lt               less than
<=, \\le              less or equal
>, \\gt               greater than
>=, \\ge              greater or equal
==================== ==================

Set Operators
+++++++++++++

.. code-block:: felix

  cmp := "in" =># '(nos "\\in")';
  cmp := "\in" =># "(nos _1)";
  cmp := "\notin" =># '(nos _1)';
  cmp := "\owns" =># '(nos _1)';

  x[ssetunion_pri] := x[ssetunion_pri] "\cup" x[>ssetunion_pri] =># "(Infix)" note "setunion";
  x[ssetintersection_pri] := x[ssetintersection_pri] "\cap" x[>ssetintersection_pri] =># "(Infix)" note "setintersection"; 

Arithmetic Summary
++++++++++++++++++

The usual operators:

==================== ==================
operator             numeric semantics
==================== ==================
\+                   addition
\-                   subtraction
\*                   multiplication
/                    division
%                    remainder
-, neg               negation
==================== ==================


Bitwise Operations
++++++++++++++++++

.. code-block:: felix

  //$ Bitwise or, left associative.
  x[sbor_pri] := x[sbor_pri] "\|" x[>sbor_pri] =># "(Infix)";

  //$ Bitwise xor, left associative.
  x[sbxor_pri] := x[sbxor_pri] "\^" x[>sbxor_pri] =># "(Infix)";

  //$ Bitwise exclusive and, left associative.
  x[sband_pri] := x[sband_pri] "\&" x[>sband_pri] =># "(Infix)";

  //$ Bitwise left shift, left associative.
  x[sshift_pri] := x[sshift_pri] "<<" x[>sshift_pri] =># "(Infix)";

  //$ Bitwise right shift, left associative.
  x[sshift_pri] := x[sshift_pri] ">>" x[>sshift_pri] =># "(Infix)";

The usual C operators spelled differently: note
for numeric types these operations only apply to
unsigned integers.

==================== ==================
operator             numeric semantics
==================== ==================
\|                   bitwise or
\^                   bitwise exclusive or
\&                   bitwise and
~                    ones complement
==================== ==================

Composition Sumary
++++++++++++++++++

There are two composition operators for functions,
both are left associative:

==================== ==================
operator             semantics
==================== ==================
\\circ               forward composition
\\odot               reverse composition
==================== ==================




Atomic Forms
++++++++++++

Grouping is provided by parenthesis:

.. code-block:: felix

   (a + b) / c


Conditional
-----------

The standard boolean conditional expression:

.. code-block:: felix

  if cond then texpr else fexpr endif

The `else` clause is mandatory, the `endif` can sometimes be dropped
if the end of the alternate expression is established by context.
The `cond` expression must be of type `bool` and `texpr` and `fexpr`
must be the same type.


Pattern Match
-------------

Pattern match expression:

.. code-block:: felix

  match mexpr with
  | pattern1 => expr1
  | pattern2 => expr2
  ...
  endmatch

The `endmatch` is mandatory. All the RHS expressions must have
the same type. All the patterns must also match the same type
as `mexpr`. If all the patterns fail to match the program
terminates with a match failure.


Applications
------------

Felix has a large number of application operators, from
highest precedence to lowest:

.. code-block:: felix

  #f   // constant evaluator: means f ()
  a.f  // operator dot: reverse application, left associative
  f a  // operator whitespace, left associative
  f$a  // operator dollar, right associative
  a|>f // operator pipe apply: reverse application, left associative

There are also two special combinations:

.. code-block:: felix

   a*.f  // means (*a).f
   a&.f  // means (&a).f

The rich collection of operators is indended to reduce the
need for parentheses.

Another application for binary operator is

.. code-block:: felix

  a `(f) b // means f (a,b)

