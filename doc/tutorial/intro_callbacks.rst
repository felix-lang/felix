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
    println$ "x="+x.str + ",y=" + y.str; 
  }
 
Now, when you register your callback pass `cb1` or `cb2` as the callback,
and pass `C_hack::cast[address]cback1` or `C_hack::cast[address[cback2`
as the client data pointer.

Complete Example
----------------

TODO.

