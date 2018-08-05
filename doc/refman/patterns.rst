Pattern Matching
================

Felix provides an advanced pattern matching system which
includes generic patterns and user defined patterns.

Pattern matching provides a way to decode a data structure
by providing an image of the type with "holes" in it which
are indicated by variables. Provided pattern matches the value
the variables take on the value of the part of the type
which is missing.

For products, pattern variables are projections of the
value, possibly chained together in reverse order.

For sum types including variants, the argument of
the constructor which created the value is extracted,
after checking the value was indeed made by the
corresponding injection function. If not, the next pattern
is tried.

A pattern match over a product type is said to be irrefutable
because it cannot fail after static type checking; however
a pattern it is included in may fail, and a pattern matching
a component may also fail.



Matches
-------

Syntax
^^^^^^

.. code-block:: felix

  syntax patterns {

    block = match_stmt;

    smatch_head := "chainmatch" sexpr "with" stmt_matching+ =># "`(,_2 ,_4)";
    smatch_link := "ormatch" sexpr "with" stmt_matching+ =># "`(,_2 ,_4)";
    smatch_chain := smatch_chain smatch_link =># "(cons _2 _1)"; // revsersed
    smatch_chain := smatch_link =># "`(,_1)";

    match_stmt := smatch_head smatch_chain "endmatch" ";" =># 
      "`(ast_stmt_chainmatch ,_sr ,(cons _1 (reverse _2)))"
    ; 

    match_stmt := smatch_head "endmatch" ";" =># 
      "`(ast_stmt_match (,_sr ,_1))"
    ; 

    //$ Pattern match statement.
    //$ At least one branch must match or the program aborts with a match failure.
    match_stmt:= "match" sexpr "with" stmt_matching+ "endmatch" ";" =>#
      "`(ast_stmt_match (,_sr ,_2 ,_4))";

    match_stmt:= "match" sexpr "do" stmt_matching+ "done" =>#
      "`(ast_stmt_match (,_sr ,_2 ,_4))";

    //$ A single branch of a pattern match statement.
    //$ The match argument expression is compared to the pattern.
    //$ If it matches any contained pattern variables are assigned
    //$ the values in the corresponding possition of the expression,
    //$ and the statements are executed.
    private stmt_matching := "|" spattern "=>" stmt+ =># "`(,_2 ,_4)";

    //$ Pattern match expression with terminator.
    satom := pattern_match "endmatch" =># "_1";

    //$ Pattern match expression without terminator.
    //$ Match the expression against each of the branches in the matchings.
    //$ At least one branch must match or the program aborts with a match failure.
    pattern_match := "match" sexpr "with" smatching+ =>#
      "`(ast_match ,_sr (,_2 ,_4))";

    //$ The match argument expression is compared to the pattern.
    //$ If it matches any contained pattern variables are assigned
    //$ the values in the corresponding possition of the expression,
    //$ and expression is evaluated and becomes the return value
    //$ of the whole match. 
    smatching := "|" spattern "=>" x[let_pri] =># "`(,_2 ,_4)";

    //$ Match nothing.
    smatching := "|" "=>" sexpr =># "`((pat_none ,_sr) ,_3)";

Patterns
--------

Guard pattern
+++++++++++++

Syntax
^^^^^^

.. code-block:: felix

    spattern := sguard_pattern ("|" sguard_pattern)* =># "(chain 'pat_alt _1 _2)";
    sguard_pattern := swith_pattern "when" sexpr =># "`(pat_when ,_sr ,_1 ,_3)";
    sguard_pattern := swith_pattern =># "_1";


With pattern
+++++++++++

Syntax
^^^^^^

.. code-block:: felix

    swith_pattern := sas_pattern "with" spat_avars =># "`(pat_with ,_sr ,_1 ,_3)";
      spat_avar := sname "=" stypeexpr =># "`(,_1 ,_3)";
      spat_avars := list::commalist1<spat_avar> =># "_1"; 
    swith_pattern := sas_pattern =># "_1";

As pattern
++++++++++

Syntax
^^^^^^

.. code-block:: felix

    sas_pattern := scons_pattern "as" sname =># "`(pat_as ,_sr ,_1 ,_3)";
    sas_pattern := scons_pattern =># "_1";

Cons pattern
++++++++++++

Syntax
^^^^^^

.. code-block:: felix

    scons_pattern := stuple_cons_pattern "!" scons_pattern 
    scons_pattern := stuple_cons_pattern 
    scons_pattern :="[" slist_pattern "]" 
    scons_pattern :="[" "]"

List pattern
++++++++++++

Syntax
^^^^^^

.. code-block:: felix

    slist_pattern := scoercive_pattern "," slist_pattern
    slist_pattern := scoercive_pattern 
    slist_pattern := scoercive_pattern ",," scoercive_pattern

Tuple Cons Pattern
++++++++++++++++++

Syntax
^^^^^^

.. code-block:: felix

    stuple_cons_pattern := stuple_pattern ",," stuple_cons_pattern =>#
    stuple_cons_pattern := stuple_pattern "<,,>" stuple_cons_pattern =>#
    stuple_cons_pattern := stuple_pattern =># "_1"

Tuple Pattern
+++++++++++++

Syntax
^^^^^^

.. code-block:: felix

    stuple_pattern := scoercive_pattern ("," scoercive_pattern )* =>#

Coercive Pattern
++++++++++++++++

Syntax
^^^^^^

.. code-block:: felix

    scoercive_pattern := sapplicative_pattern "|>" x[sarrow_pri]
    scoercive_pattern := sapplicative_pattern ":" x[sarrow_pri]
    scoercive_pattern := sapplicative_pattern
    scoercive_pattern := stypeexpr ":>>" sname

Applicative Pattern
+++++++++++++++++++

Syntax
^^^^^^

.. code-block:: felix

    sapplicative_pattern := sctor_name sargument_pattern 

    private sapplicative_pattern := sctor_name stypeexpr+ sargument_pattern 

      //$ The sum type constructor can either be a qualified name...
      private sctor_name := sname

      //$ or it can be a case literal.
      sctor_name := "case" sinteger
      sctor_name := "`" sinteger


    sapplicative_pattern := "case" sname sargument_pattern
    sapplicative_pattern := "`" sname sargument_pattern
      sargument_pattern := satomic_pattern
    sapplicative_pattern := satomic_pattern

Atomic Pattern
++++++++++++++

Syntax
^^^^^^

.. code-block:: felix

    satomic_pattern := sname 
    satomic_pattern := "?" sname 
    satomic_pattern := "val" sname 
    satomic_pattern := "#" sctor_name
    satomic_pattern := "#" "case" sname
    satomic_pattern := "`" sname
    satomic_pattern := "case" sinteger
    satomic_pattern := "`" sinteger

    satomic_pattern := "true"
    satomic_pattern := "false"

    satomic_pattern := "_"
    satomic_pattern := "(" spattern ")" 
    satomic_pattern := "(" ")" 

    satomic_pattern :=  "(" spat_assign ("," spat_assign )* ")"
      spat_assign := sname "=" spattern =># "`(,_1 ,_3)";

    satomic_pattern :=  "(" spat_assign ("," spat_assign )* "|" sname ")" 

    satomic_pattern := "$" "(" sexpr ")" 
    satomic_pattern := sliteral 
    satomic_pattern := sliteral ".." sliteral 

  }


