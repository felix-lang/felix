Records
=======

Felix allows you to create record values on the fly like this:

.. code-block:: felix

    var x = (a=1, b="hello", c=4.2);
    println$ x._strr;

The builtin function `_strr` can be used to translate a record
value to a string.

The record value `x` we specified above has the record type:


.. code-block:: felix

    (a:int, b:string, c:double)


