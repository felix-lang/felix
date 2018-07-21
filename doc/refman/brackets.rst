Bracket Forms
=============

.. code-block:: felix

  //$ Array expression (deprecated).
  satom := "[|" sexpr "|]" 

  //$ Short form anonymous function closure.
  satom := "{" sexpr "}" 

  //$ Grouping.
  satom := "(" sexpr ")" 
  satom := "\(" sexpr "\)"
  satom := "\[" sexpr "\]"
  satom := "\{" sexpr "\}" 

  //$ floor and ceiling
  satom := "\lceil" sexpr "\rceil"
  satom := "\lfloor" sexpr "\rfloor"

  //$ absolute value
  satom := "\lvert" sexpr "\rvert" 
  satom := "\left" "|" sexpr "\right" "|" 
  satom := "\left" "\vert" sexpr "\right" "\vert" 

  //$ norm or length
  satom := "\lVert" sexpr "\rVert" 
  satom := "\left" "\Vert" sexpr "\right" "\Vert"

  // mediating morphism of a product <f,g>
  satom := "\langle" sexpr "\rangle" 
  satom := "\left" "\langle" sexpr "\right" "\rangle" 

  // mediating morphism of a sum [f,g]
  satom := "\lbrack" sexpr "\rbrack" 
  satom := "\left" "\lbrack" sexpr "\right" "\rbrack" 



