Class Instances
===============

Our `Eq` class defines a simple algebra which specifies an interface for
equivalence relations together with their semantics. We have to use
an `instance` to populate the algebra.

.. code-block:: felix

  instance Eq[int] {
    fun == int * int -> bool = "$1==$2";
  }

Here we have defined an instance of the Eq class for the argument type `int`.
Our code *implements* the Eq class interface, by
defining all the non-default virtual methods. 

In this case the definition is done by delegating the implementation to C++,
using a binding.

We would like equality for `double` as well:

.. code-block:: felix

  instance Eq[double] {
    fun == double * double -> bool = "$1==$2";
    fun != double * double -> bool = "$1!=$2";
  }

Here we choose to also define the `!=` function as well, 
overriding the default provided in the class.

When you provide instances, it is usual to ensure there is a
definition for all the class methods. Care must be taken
to avoid circular definitions, because that will lead
to inifinite recursions at run time.




