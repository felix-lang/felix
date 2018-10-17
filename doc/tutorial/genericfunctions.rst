Generic Functions
=================

Generic functions and procedures provide a simple definition.

.. code-block:: felix

  // generic function
  fun add3 (x,y,x) => x + y + z;

  // used with different argument types
  println$ add3 (1,2,3); // 6
  println$ add3 (1.0,2.0,3.0); // 6.0
  println$ add3 ('Hello',' ','World'); // Hello World

For each uses of a generic function, Felix makes a copy
and adds the argument types. So the three calls above
actually call these automatically generated functions:

.. code-block:: felix

  add3 (x:int, y:int, z:int) => x + y + z;
  add3 (x:double, y:double, z:double) => x + y + z;
  add3 (x:string, y:string, z:string) => x + y + z;

Note that the rewritten functions are generated in the same
scope as the generic function so any names used in the generic
function refer to the names in the generic function's original
scope. 
