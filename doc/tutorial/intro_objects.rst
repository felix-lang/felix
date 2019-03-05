Objects
=======

Basic Construction
------------------

Felix provides a dynamic object construction mechanim.

.. code-block:: felix

 object ob (x:string) = {
     var age = 0;
     method fun name () => x;
     method fun underage () => age < 21;
     method proc setage (y:int) { age = y; }
  };
  var joe = ob "joe";
  joe.setage 42;
  println$ "name " + joe.name() + " is " + 
    if joe.underage() 
    then "underage" 
    else "of age" 
    endif
  ; 

Here, `ob` is an ordinary function with one special property:
the return is done automatically, and the returned value is
a record of closures of the functions and procedures
specified as `method`. They are each closed over the local
data frame of the *constructor* function `ob`.

Applying the object constructor `ob` to an argument that matches
its parameter `x` creates the actual object, which is an ordinary
record value.

Object type
-----------

The type of the object above is given by:

.. code-block:: felix

  typedef ob_t = 
  (
    name: 1->string,
    underage: 1->bool,
    setage: int -> 0
  );


An alternative syntax is:

.. code-block:: felix

  interface ob_t  {
    name: 1->string;
    underage: 1->bool;
    setage: int -> 0;
  };

You can use this to specify the return type of the constructor:

.. code-block:: felix

 object ob (x:string) implements ob_t = {
     var age = 0;
     method fun name () => x;
     method fun underage () => age < 21;
     method proc setage (y:int) { age = y; }
  };


Dynamics
--------

Felix objects are more flexible than Java or C++ because they're not
defined by classes, they're just values contructed by a function.
You can, in fact, constuct an object with an ordinary function:
the only magic with an `object` bound function is the automatic
return of the record of `method` closures.

Because an instance object is just a record, you can apply any
record operations to it!

However, the local variables of the constructor to which the method
closures are bound are protected by functional abstraction, and so
can be considered private. This privacy cannot be bypassed.

But you can do this:

.. code-block:: felix

  var joe = ob "joe";
  joe = (joe with name = (fun () => "Joe")); // change name method
  println$  "name " + joe.name(); // prints Joe not joe

  var xjoe = extend joe with (surname = "Blogs") end;
  println$ xjoe.surname;

Note, the type of `xjoe` is no longer the same, it has an extra field
called `surname`. This field is just a string, not a method.


