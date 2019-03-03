Struct Subtyping
================

Felix structs do not provide any inheritance mechanism.
However, you can define a subtyping coercion:

.. code-block:: felix

  struct Point {
    x: int;
    y: int;
    fun norm => max (self.x,self.y);
    proc reset { self.x <- 0; self.y <- 0;}
    proc set (a:int, b:int) { self.x <- a; self.y <- b; } 
  }

  struct ColouredPoint {
    p: Point;
    colour : int;
  }

  var colp = ColouredPoint (p, 5);

  supertype Point (cp: ColouredPoint) => Point ( cp.p.x, cp.p.y);

  println$ colp.norm.str;

The coercion has the parameter of subtype ColouredPoint and returns
a value of the supertype Point. Now the `norm` method which takes
a point will work with a ColouredPoint too, because it is a subtype.

Note that the mutating methods will *not* work because subtyping
does *not* extend from values to read-write pointers. In fact,
whilst read-pointers are covariant, write pointers are actually
contra-variant.

Finally note, explicit subtyping coercions given by the `supertype`
construction are not transitive. Although in theory, subtyping
is transitive, the `supertype` construction does not chain
coercions together. If you have defined a coercion from A to B,
and from B to C, then if you want A to act as a subtype of C,
you have to explicitly define a coercion from A to C.
