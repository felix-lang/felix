Binding Callbacks
=================

C code often uses callbacks. Callbacks always consist of two type of functions:

  1. A registration function
  2. The callback function

The registration function is called with the callback function as an argument,
and it stores the callback inside a server.

Then when some specified kind of event occurs, the callback is invoked
by the server.

Callback functions almost always have a special parameter which is
typically called `client_data` and is a `void*` to some arbitrary data.
When the callback is registered, a particular client data pointer is
accepted as well. When the callback is invoked, the server passes the
registered client data pointer to it.

Felix provides a way to write C callbacks in Felix. To do this,
a fixed function is generated, of the type required for the 
callback, which casts the client data pointer to a closure of
the Felix function implementing the callback, and then invokes it.

Both procedural and function callbacks are supported. The Felix
code has full access to the garbage collector, but must not
actually trigger a collection. Service calls cannot be done
because the C stack is in the way.

The callback is defined like:

.. code-block:: felix

  callback fun cb1 : int * cb1 * double -> int;

  callback proc cb2 : int * cb2 * double;
 
Here the special parameter which is the name of the callback type
given, `cb1` and `cb2` in the example, indicates the position of the client 
data pointer. 

These functions will be generated in the C code:

.. code-block:: c

  int cb1(int, void *client_data, double);

  void cb2(int, void *client_data, double);

To actually define the callbacks in Felix:

.. code-block:: felix

  fun cback1 (x:int, y:double) => (x.double + y).int;

  proc cback2 (x:int, y:double) { 
    println$ "x=" + x.str + ",y=" + y.str;
  }
 
Now, when you register your callback pass `cb1` or `cb2` as the callback,
and pass `C_hack::cast[address]cback1` or `C_hack::cast[address]cback2`
as the client data pointer.

Complete Example
----------------

A function callback with an `int` argument.

.. code-block:: felix
  body demo =
  """
  int register_cback(int x, void *client_data, int (*cback)(int, void*))
  {
    // for pedagogical purpose directly call the callback
    return cback(x, client_data);
  }
  """;

  fun register_cback: int * address * (int * address --> int) -> int
    requires demo;

  callback fun cback_wrapper : int * cback_wrapper -> int;

  fun increment (x:int) => x + 1;

  var x = register_cback(
    8,
    C_hack::cast[address] increment,
    C_hack::cast[int * address --> int] cback_wrapper
  );

  println$ "Callback function returned " + x; // 9


Procedure callbacks with an `int` and `double` arguments.

.. code-block:: felix
  body demo =
  """
  void register_cback(
    int x,
    double y,
    void *client_data,
    void (*cback)(int, double, void*))
  {
    cback(x, y, client_data);
  }
  """;

  proc register_cback: int * double * address * (int * double * address--> void)
    requires demo;

  callback proc cback_wrapper : int * double * cback_wrapper;

  proc add (x:int, y:double) {
    println$ x.double + y;
  }

  proc sub (x:int, y:double) {
    println$ x.double - y;
  }

  register_cback(
    8,
    1.5,
    C_hack::cast[address] add,
    C_hack::cast[int * double * address --> void] cback_wrapper
  ); // prints 9.5

  register_cback(
    8,
    1.5,
    C_hack::cast[address] sub,
    C_hack::cast[int * double * address --> void] cback_wrapper
  ); // prints 6.5
