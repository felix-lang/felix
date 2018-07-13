Generators
==========

A generator is a special function like construction which is
permitted to have an *internal* effect. The prototypical
generator is the rand() function.

An internal effect is one which changes some state on each call
and which my modify the result returned by a subsequent call,
the state, may not be observed except in the return value
of the function.

A generator is defined by using the binder "gen" instead of "fun".

Note that the type of a generator is the same as function type.

Iterators
---------

Felix has a special role for generators named iterator: they're
used to traverse a data structure such as a list, array, tree
or other type, visiting certain values in some order, and returning
each such value in sequence on each call. In C++ terminology 
an iterator is an input iterator.

Yielding Generators
-------------------

Most generators maintain internal mutable state in local variables.
In order to preserve modification between applications, as well
as the current location of the program counter, they execute
a yield statement to return a value. Subsequent calls to the
generator continue after the yield statement.

To use a yielding generator, a closure must be assigned to
a variable to hold the state. For example:

.. code-block:: felix

  gen iterator(xs:T^N) () : opt[T] =
  {
    if xs.len > 0uz do
      for var j in 0uz upto xs.len - 1uz do
        yield Some (xs,j).unsafe_get;
      done
    done
    return None[T];
  }

  proc check() {
    var a = 1,2,3,4;
    var it = iterator a;
  next:>
    var v = it ();
    match v with 
    | Some x => 
      println$ x; 
      goto next;
    | None => ;
    endmatch;

defines and uses an array iterator.



