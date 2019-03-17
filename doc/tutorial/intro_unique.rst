Uniqueness Types
================

Felix has a special type combinator `uniq` which is used to specify the value of its 
argument type is exclusively owned. It is usually applied to pointers.

First lets write a procedure to convert a varray of chars to upper case:

.. code-block:: felix

  proc toupper(x: varray[char]) {
    for i in 0 ..< x.len.int
      perform set(x,i,toupper(get(x,i)));
   }

Varray's are passed by reference, so this modifies the varray in place.
So how would we use this in a function?

.. code-block:: felix

  fun upcase(x: varray[char]) {
    var y = varray x; // copy
    toupper y;
    return y;
   }


We can't just call toupper on the parameter x and return it because
in Felix functions are not allowed to have side effects.

But consider this case:

.. code-block:: felix

   var upped = upcase$ varray("Hello World");
   println$ upped.str;

This is inefficient because in `upcase` we copy the array,
modify the copy and return it, but the argument `x` is then no longer
reachable, so it will be garbage collected.

Why not just modify the argument directly in this case:

.. code-block:: felix

  fun upcase(x: uniq varray[char]) {
    toupper x,peek;
    return x;
   }

   var upped = upcase$ box (varray("Hello World"));
   println$ upped.str;


