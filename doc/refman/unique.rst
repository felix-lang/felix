Uniqueness Types
================

Felix provides a special type constructor `_uniq` which can
be used to indicate ownership of a value, typically a pointer.
Here is a synopsis of the user interface:

.. code-block:: felix

  open class Unique 
  {
    // box up a value as a unique thing
    fun box[T] : T -> _uniq T = "($t)";

    // unsafely unpack the unique box
    fun unbox[T] : _uniq T -> T = "($t)";

    // kill a live unique value
    proc kill[T] : uniq T = ";";

    // functor for typing
    typedef fun uniq (T:TYPE):TYPE => _uniq T;

    // peek inside the box without changing livenes state
    fun peek[T] : &<(uniq T) -> T = "*($t)";
  }

The intention is as follows: a function, say `f`, can specify
a uniquely typed argument so that it can claim exclusive
ownership of the value. Since all others are now excluded
from access to the value, the function can then modify
the value without the mutation being considered a side-effect,
thus preserving referential transparency.

The function can then pass the modified value on "as if"
it had made a local copy which it would necessarily
have unique access to, and modified that value. The copy
would be required so accesses external to the value
continued to see the original value, however since
such accesses are disallowed, the original can
safely be modified.

The client of the function cannot pass a plain value to
the function because that would be a type error: instead
the client has to box the value to make it acceptable
to the receiver function. By doing this the programmer
is forced by the type system to become aware that the client
must relinquish access to the value after calling the
function.

Felix does not enforce safety in that the client may box
a copy of a value and then access the original. 

The client on the other hand cannot use the value until
it is removed from its box, so the client programmer
again is forced to take a specific action to take
responsibility for correct handling.

In other words, the uniqueness typing system enforces
a contract in which both sender and receiver must take 
positive action to comply with the transfer.

Felix provides a limited first order guarrantee that transfer
of ownership of values stored in variables respects move 
semantics. First order means the assurance does not extend
to indirections, either via pointers, or via closures.
The limitation is that the checks may not work correctly
in abnormal circumstances. For example, a uniq value
must normally be either killed or ownership transfered,
however in case of a program exit Felix may not detect
a failure to kill a unique value.

The check Felix does is based on static flow control
analysis. The general rule is that if a variable
has a unique type, then if the variable is dead
it can be written which livens it, if it is
live then it must be read exactly once, which kills it.

Felix ensures that if a live variable is moved to one
branch of a conditional or pattern match, it is moved
to all of them (except in case of a match failure which
will abort the program).

Dually, Felix ensures that if a variable is assigned
to a component of a variable of product type,
that component is treated "as if" it were a whole variable.

A more detailed discussion with examples can be 
found here: https://modern-computing.readthedocs.io/en/latest/unique.html.

 
