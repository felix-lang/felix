
Assignments
+++++++++++

Felix primary method of setting store is the intrinsic `_storeat`:

.. code-block:: felix

  proc storeat[T] (p: &>T, v:T) { _storeat (p,v); }

The library procedure take a pointer or write-only point to T
and a value V of type T, and calls the system intrinsic _storeat.
The parser in turn maps

.. code-block:: felix

  p <- v;

to the procedure `storeat`. For simple variables only you can write:

.. code-block:: felix

  x = v;

which is notionally sugar for

.. code-block:: felix

  &x <- v;

In addition each of the following infix operators calls a two argument
procedure with the same name as the operator:

======== ===========================
operator usual meaning for uints
======== ===========================
+=       increment
-=       decrement
/=       quotient
\*=      product
%=       remainer
\<\<=    mul 2^N
\>\>=    div 2^N
\^=      bitwise exclusive or
\&=      bitwise and
\|=      bitwise or
======== ===========================



