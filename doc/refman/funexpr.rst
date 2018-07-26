Functional Expressions
======================

In Felix, a function is modelled in principle by a C++ class.
A function value, on the other hand, is not a function,
rather it is a called a closure because it captures its environment
and may have internal state. Closures are represented as pointers
to objects of some C++ class.

The distinction is important. In principle in an application `f a`
the `f` is a function value of some suitable type. The type given for
a function in a definition is the type of its closure. However if the
function expression is just a name, overload resolution is performed
to find a suitable function to apply, and the application is direct
so that the function generating the closure being applied is known,
and the application is optimised, for example, by inlining the application.

Function names
--------------

Syntax
^^^^^^

.. code-block:: felix

  ssuffixed_name := squalified_name "of" x[sthename_pri]

Description
^^^^^^^^^^^

The full name of a function defined by the user can be given
with a suffixed name. For example:

.. code-block:: felix

  class X { fun f(x:int) => x; }
  var g = X::f of int;

The `of` suffix is used in lieu of an argument to perform overload resolution
and select a specific function.

This syntax is also used to name union constructors.

Tuple projections
-----------------

Syntax
^^^^^^

.. code-block:: felix

  x[scase_literal_pri] := "proj" sinteger "of" x[ssum_pri]

Description
^^^^^^^^^^^

You can name a specific projection of a tuple type by:

.. code-block:: felix

  typedef t = int * long * string;
  var g : t -> string = proj 2 of t;

Array projections
-----------------

.. code-block:: felix

  typedef t = int^5;
  var g : t -> 5 -> int = aproj of t;

[NOTE: THIS ISNT IMPLEMENTED BUT SHOULD BE]


Record and struct projections
-----------------------------

Records and structs use the field name as the name of the projection,
so the usual suffixed form can be used to specify a projection.

.. code-block:: felix

  typedef t = (a:int, b:long, c:string);
  var g : t -> string = a of t;
  struct X { a:int; b:long; c:string);
  var h : X -> string = a of X;

Sum Injections
--------------

Syntax
^^^^^^

.. code-block:: felix

  x[scase_literal_pri] := "case" sinteger "of" x[ssum_pri] 
  x[scase_literal_pri] := "`" sinteger "of" x[ssum_pri] 
  x[scase_literal_pri] := "`" sinteger ":" x[ssum_pri] 


Coarray Injection
-----------------

Syntax
^^^^^^

.. code-block:: felix

  // coarray injection
  // (ainj (r:>>4) of (4 *+ int)) 42
  x[scase_literal_pri] := "ainj"  stypeexpr "of" x[ssum_pri] =># "`(ast_ainj ,_sr ,_2 ,_4)";


Compositions
------------

Forward and reverse serial, parallel, mediating morphisms.

.. code-block:: felix

  //$ Reverse composition
  x[srcompose_pri] := x[srcompose_pri] "\odot" x[>srcompose_pri]

  //$ Forward composition
  x[ssuperscript_pri] := x[ssuperscript_pri] "\circ" x[>ssuperscript_pri]

  // ???? 
  x[ssuperscript_pri] := x[ssuperscript_pri] "\cdot" x[>ssuperscript_pri]

Categorical Constructions
^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: felix

  // mediating morphism of a product <f,g>
  satom := "\langle" sexpr "\rangle" =># "`(ast_apply ,_sr (,(noi 'lrangle) (,_2)))";
  satom := "\left" "\langle" sexpr "\right" "\rangle" =># "`(ast_apply ,_sr (,(noi 'lrangle) (,_3)))";

  // mediating morphism of a sum [f,g]
  satom := "\lbrack" sexpr "\rbrack" =># "`(ast_apply ,_sr (,(noi 'lrbrack) (,_2)))";
  satom := "\left" "\lbrack" sexpr "\right" "\rbrack" =># "`(ast_apply ,_sr (,(noi 'lrbrack) (,_3)))";
 

.. code-block:: felix

  fun f(x:int) => x.str;
  fun g(x:int) => x.double+42.1;

  // mediating morphism of product
  println$ \langle f , g\rangle 1; // ("1", 43.1)

  // parallel composition
  println$ \prod (f , g) (1,2); // ("1", 44.1)

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




Lambda Forms
------------

A unit function or procedure can be written inline, anonymously:

.. code-block:: felix

    // functions
    { 42 }                        // 1->int
    { var x = 1; x * x }          // 1->int
    { var x = 1; return x * x; }  // 1->int

    // procedure
    { var x = 1; println$ x; }    // 1->0

A useful construction:

.. code-block:: felix

    {
      var x = 1;
      println$ "Hello";
    };

looks like a block in C except for the terminating `;`.
Actually it is a call to an anonymous procedure since the
`call` can be elided, and the argument `()` can also be
elided. You can jump out of an anonymous procedure
but not into it, since it creates a scope. You cannot
jump out of functions, and thus not anonymous functions either.


Functions or procedures with arguments can be
written too:


.. code-block:: felix

  (fun (x:int)=>x * x)
  (proc (x:int){println$ x;})

The enclosing parens are not part of the syntax but are
often required to get the precedence right.





