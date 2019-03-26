Row Polymorphism
=================

Polyrecord Parameters
---------------------

Row polymorphism provides an alternative to record subtyping
where instead of the subtype coercion forgetting extraneous fields,
they're preserved as group, even though the client function does not
individually know them. Here's a simple example:

.. code-block:: felix

  typedef point = (x:int, y:int);
  typedef coloured_point = (x:int, y:int, colour: int);
  typedef elevated_point = (x:int, y:int, z:int);

  fun step_right[T] (a : (x:int, y:int | T)) => (a with x=a.x+1);

  var cp : coloured_point = (x=0, y=0, colour=42);
  var ep : elevated_point = (x=0, y=0, z=100);

  println$ cp.step_right._strr;
  println$ ep.step_right._strr;

Here, the type T will be a record type containing all the fields
of the argument type other than `x` and `y`. This can vary
from call to call. Unlike subtyping, the actual argument
is updated with a functional update which preserves all the
original fields, even ones the function doesn't know about.

The type of the parameters here is called a polyrecord type,
it consists of the usual record field list and a vertical bar
followed by another type, which can be a type variable, as
in the example.

Polyrecord values
-----------------

Polyrecord types are ordinary types, you can define values for them:

.. code-block:: felix

   var a = (x=1, y=2 | (e=42));

The type of the value on the RHS of the vertical bar must 
resolve to a record, tuple, array, or unit after monomorphisation,
including a polyrecord which so resolves, recursively.

Use with Objects
----------------

Because object types are just record types row polymorphism can be
used with objects. For example:

.. code-block:: felix

  object coloured_point (var x:int, var y:int, var colour: int) {
    method fun getx () => x;
    method proc setx (newx: int) { x = newx; }
    method proc show { println$ "x=" + x.str + ", y="+y.str + ",colour=" + colour.str; }
  };

  proc step_right[T] (a : (getx: 1-> int, setx: int->0 | T)) {
    a.setx (a.getx() + 1);
  }

  var cp = coloured_point (x=0,y=0,colour=42);
  cp.step_right;
  cp.show();




