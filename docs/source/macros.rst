Macro processing
================

`Syntax <http://felix-lang.org/share/lib/grammar/macros.flxh>`_

`Semnantics <http://felix-lang.org/share/src/compiler/flx_desugar/flx_macro.ml>`_

Macro val
---------

The macro val statement is used to specify an identifier should
be replaced by the defining expression wherever it occurs in an
expression, type expression, or pattern.

.. code-block:: felix
   
   macro val WIN32 = true;
   macro val hitchhiker;
   macro val a,b,c = 1,2,3;


Macro for
---------

This statement allows a list of statements to be repeated
with a sequence of replacements.

.. code-block:: felix
   
   forall name in 1,2,3 do
     println$ name;
   done

Constant folding and conditional compilation
--------------------------------------------

`Reference <http://felix-lang.org/share/src/compiler/flx_desugar/flx_constfld.ml>`_

Felix provides two core kinds of constant folding:
folding of arithmetic, boolean, and string values, and 
deletion of code, either statements or expressions,
which would become unreachable due to certain
value of conditionals.

Basic operations on integer literals, namely 
addition, subtraction, negation, multiplication,
division, and remainder are folded.

Strings are concatenated.

Boolean and, or, exclusive or, and negation,
are evaluated.

False branches of if/then/else/endif expression
and match expressions are eliminated.

False branches of if/do/elif/else/done 
are also eliminated.

By this mechanism of constant folding and
elimination, Felix provides conditional
compilation without the need for special
constructions.

