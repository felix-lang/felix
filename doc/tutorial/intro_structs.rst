Structs
=======

Definition
++++++++++

The `struct` construction introduces a nominally typed, or named product.

.. code-block::

  struct Point {
    x: int;
    y: int;
  }

  instance Str[Point] {
    fun str (p:Point) => 
      "Point(" + p.x.str + ", " + p.y.str + ")"
    ;
  }

  var p = Point (2,3);
  println$ p.str;

Here, we not a Point consists of two integers, x and y, and we can make a Point
by applying the type name Point to a pair of integers.

Also we provide an instance of the `Str` class with method `str`
or type `Point->string` for converting a point to a human readable
form. `Str` is a standard library class which is universally open.

Methods
+++++++

Struct can have methods of two kinds: `accessors` and `mutators`.
Here is an expanded version of Point:

.. code-block::

  struct Point {
    x: int;
    y: int;
    fun norm => max (self.x, self.y);
    proc reset { self.x <- 0; self.y <- 0; }
    proc set (a:int, b:int) { self.x <- a; self.y <- b; } 
  }
  var p = Point (1,2); 
  var y = p.norm;
  p.set(p.x + 1, p.y + 1);
  

Accessors, with `fun` binder, implicitly get an extra parameter `self`
of type `Point`. Mutators, with `proc` binder, implicitly get an extra
parameter `self` of type `&Point`, a pointer to a Point.

Object Closures
+++++++++++++++


In fact, these methods are ordinary functions and procedures.
The nesting is just syntactic sugar for:

.. code-block::

  struct Point {
    x: int;
    y: int;
  }

  fun pythag (self: Point) => max(self.x, self.y);
  proc reset (self: &Point) { self.x <- 0; self.y <- 0; }
  proc set (self: &Point) (a:int, b:int) { self.x <- a; self.y <- b; } 

Because of this, the methods are higher order functions and we can form closures
over objects of type Point:

.. code-block::

  var p = Point (1,2);

