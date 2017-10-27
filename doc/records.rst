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

Fields
~~~~~~

The fields of a record can be accessed by name:

.. code-block:: felix

    var x = (a=1, b="hello", c=4.2);
    println$ x.a;
    println$ a x;

The field name is used as a projection function, it is a first
class function. If two records have the same field name, the
function is selected by the usual overloading process:

.. code-block:: felix

    var x = (a=1, b="hello", c=4.2);
    var y = (b=42, d=99);
    println$ x.b; // selects the string
    println$ b y; // selects the integer
    var prj = b of (b:int, d:int);
    println$ prj y;

Duplicate Fields
~~~~~~~~~~~~~~~~

Records may have duplicate fields:

.. code-block:: felix

    var x = (a=1, a="hello",a=42);
    println$ 

In this case the first or leftmost field is always selected
when the field name is used as a projection.

Duplicate fields arise naturally as a result of some record
operations.

Blank Field Names
~~~~~~~~~~~~~~~~~

The name of a field can be blank. There are three ways to indicate
a blank name in a record value:

.. code-block:: felix

    var x = (n""=1, ="hello", 42, y=1);

The first method uses an explicit identifier literal to construct
an identifer from an empty string. The second form simply omits
the identifier but keeps the = sign. The third form omits the
identifier and the = sign. All three methods are equivalent,
however the third form is not permitted at the begining of a record
value. A similar syntax may be used for record types:


.. code-block:: felix

    typedef x_t = (n"":int, string, :int, y:int);

Accessing blank fields
~~~~~~~~~~~~~~~~~~~~~~

Felix allows blank record fields to be access by number.

.. code-block:: felix

    var x = (n""=1, ="hello", 42, y=1);
    println$ x.1; // "hello"

Record with all blank fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If all the fields of a record have blank names,
then the record is a tuple. Therefore tuples are
just a special case of records, and since arrays are
a special case of tuples, arrays are also a special 
case of records.

Record Addition
~~~~~~~~~~~~~~~

Two records can be concatenated by using the infix + operator:

.. code-block:: felix

    var x = (a=1,b=2) + (a=3, d=3);

As usual if there is a duplicate field, the left field
hides any fields to its right with the same name.

Functional Update 
~~~~~~~~~~~~~~~~~

A record can be updated using functional update syntax:

.. code-block:: felix

    var x = (a=1, b=2, c=3);
    var u = (x with b=42, c=99);

The result is a new record with the values of the specified
fields replaced. Only the first field of a duplicate set
can be updated. The field must exist, and must have the same type.

Dropping Fields
~~~~~~~~~~~~~~~

A record can also be updated by removing fields:

.. code-block:: felix

    var x = (a=1, b=2, c=3, c=99);
    var u = (x without c c b);

More than one field can be removed by listing the field names
without a separating comma. If a field is duplicated only
the leftmost field is removed, the next field can be removed
by giving the same name again.

Adding Fields
~~~~~~~~~~~~~

Fields can be added on the left with `polyrecord` syntax:

.. code-block:: felix

    var x = (a=1,b=2);
    var y = (c=1,a=66 | x);


Polyrecords are a separate advanced topic discussed under the
topic `row polymorphism`.



