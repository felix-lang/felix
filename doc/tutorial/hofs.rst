Higher Order Functions
======================

In Felix, functions are first class which means a function
can be used as a value. A function which accepts another
function as a parameter, or returns a function as a result,
is called a `higher order function`, abbreviated to `HOF`.

Here's an example:

.. code-block:: felix

   fun twice (x:int):int => x + x;
   fun thrice (x:int):int => x + x + x;

   fun paired (f:int->int, a:int, b:int) =>
     f a, f b
   ;

   println$ paired (twice,2,3);
   println$ paired (thrice,2,3);

Here, the function `twice` is passed as an argument
to `paired`, binding to the parameter `f`,
which is then applies to the arguments `a` and `b` 
to obtain the final result.

Then, we do it again, this time passing `thrice`
instead.




