.. _compactlineartypes:

Compact Linear Types
--------------------

Variadic Positional Number Systems
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let :math:`a` be any non-negative number and let :math:`d` be any
positive number, then there are unique values :math:`q` and :math:`r`
such that

.. math::

  a = r + qd + {\rm\ \ such that\ } q\ge0 {\rm\ and\ } r < d

We define integer division:

.. math::

  a \operatorname{div} d = q

and remainder

.. math::

  a \operatorname{rmd} d = r

Note that in C, :code:`a/b` where the dividend and quotient
are integral is integer division and :code:`a%b` is the remainder.

Let 

.. math::

  c_0, c_1, ..., c_{n-1}

be finite sequence of positive integers called *radices*, and let

.. math::

  v_0, v_1, ..., v_{n-1}

be a sequence of non-negative integers called *coefficients* such that

.. math::

  v_i < c_i\ {\rm\ \ for\ } i \in 0..n-1

Let

.. math::

   z_0 &= 1

   z_i &= c_iz_{i-1}=\sum_{j=0}^{i-1}c_j {\rm\ \ for\ } i \in 1..n-1

so that :math:`z_i` is the product of all the :math:`c_j` for 
:math:`j<i`; these quantites are called *weights*.

Let 

.. math::

  a = \prod_{i=0}^{n-1} v_iz_i

Then the total number of values :math:`a` could take is :math:`z_{n-1}` 
and the maximum is therefore :math:`z_{n-1}-1`. 

This is known as a *variadic positional number system*.
For any number in the range 0 to the maximum, 
the coefficients :math:`v_i` are uniquely determined
and there is a bijection between the integer values and the
sequence of coefficients.

Given a value :math:`a` we would like to be able to calculate
the value :math:`v_i` for a given :math:`i`.  Let us first 
rewrite the formula for :math:`a` like this:

.. math::

  a &= \sum_{j=0}^{i-1} v_jz_j + v_iz_i + \sum_{k=i+1}^{n-1} v_kz_k\\
  &= (\sum_{j=0}^{i-1} v_jz_j) + (v_i + \sum_{k=i+1}^{n-1} v_k^(z_k/z_i))z_i

We note that this is of the required quotient and remainder form
since the left term is clearly less than :math:`z_i`, 
and, since :math:`z_i` divides `z_k` exactly for :math:`k\ge i`,
so we can find

.. math:: 

  q = a \operatorname{div} z_i = v_i  + \sum_{k=i+1}^{n-1} v_k^(z_k/z_i)

But now we can rewrite that term as well:

.. math:: 

  q &= v_i  + (\sum_{k=i+1}^{n-1} v_k^(z_k/(z_ic_i)))c_i\\
  &= v_i  + (\sum_{k=i+1}^{n-1} v_k^(z_k/(z_{i+1)))c_i

Again it is true by specification that :math:`v_i < c_i`
and :math:`z_{i+1}` divides :math:`z_k` exactly for 
:math:`k\ge i+1` which is the lowest index of the
sum, therefore since the equation has the required
quotient and remainder form:

.. math:: 

  v_i = (a \operatorname{div} z_i) \operatorname{rmd} c_i

or in C:

.. code-block:: felix

  vi = a / zi % ci

We remark that for powers of radicies which are powers of 2,
the above formula reduces to a right shift followed by mask.


We are not finished though. Let us assume that :math:`v_i` itself 
is suitably encoding a positional form using radices

.. math::

  c_0', c_1', ..., c_{m-1}'

with coefficents

.. math::

  v_0', v_1', ..., v_{m-1'}


and we want to find the :math:`v_j'`. Obviously we can 
just do this:

.. code-block:: felix

  vj' = (a / zi % ci) / zj' % cj'

by using the same formula recursively. However that formula
is not good because it uses 4 constants.  
Can we do it with just two, calculated from the four?

The intuition is very strong: instead of doing more
complex maths, visualise *inserting* the expansion of
:math:`v_i` into the original formula with parenthesis. 

Since addition is associative and multiplication distributive,
we can remove the parenthesis by multiplying the inner terms
by :math:`z_i`. Now observe the resulting formula has the same
form for a new variadic positional number system, and we only
need relabel the radices and coefficients, using a new index
from 0 through :math:`m+n-1` to obtain the original form.

Therefore we simply have to divide by :math:`z_iz_j'`, and
our remainder is just :math:`c_j'` and so:

.. code-block:: felix

  vj' = (a / zi * zj') % cj'


Compact Linear Types
^^^^^^^^^^^^^^^^^^^^

You may wonder why we did the above calculations!
In Felix, we define a compact linear type inductively as:

* unit
* any product of compact linear types
* any sum of compact linear types

Felix has special notation for sums of units. Unit can also be
written as type 1. A sum of n units can be written as n:

.. code-block:: felix

  unit = 1
  2 = 1 + 1 // aka bool
  3 = 1 + 1 + 1
  ...

These types are called *unitsums* because they're sums of units.
Using the decimal representation is more convenient that
the 1-ary representation. The type 2 is well known, it is
called `bool`.

Values of unitsums are written with a zero origin case number
and the type:

.. code-block:: felix

  `0:1 // ()
  `0:2 // false
  `1:2 // true
  `3:5 // case 3 of 5
  ...

Note again the unfortunate fact we use zero-origin case numbers
which reads badly in natural language!

We can form products of unit sums:

.. code-block:: felix

  var x : 3 * 4 * 5 = `1:3,`2:4,`3:5

for example. Now, with some luck, you might see this:

.. math::

  c0=3, c1=4, c2=5\\
  v0=1, v1=2, v2=5

and immediately recognize nothing more difficult than a variadic
positional number system! In fact this is precisely how Felix
represents a compact linear type: as a single machine word
holding an integer.

Value Projections
^^^^^^^^^^^^^^^^^

Projections for components of compact linear products use the same syntax
as for non-compact products.

.. code-block:: felix

  typedef p345_t = 3 * 4 * 5;
  var x : p234_t = `1:3,`2:4,`3:5;
  println$ x.1; // `2:4

  var p = proj 1 of (p345_t);
  println$ x.p;

You will now understand the C++ representation:

.. code-block:: cpp

  // compact linear type
  typedef ::std::uint64_t cl_t; 

  // projection
  struct RTL_EXTERN clprj_t 
  {
    cl_t divisor;
    cl_t modulus;
    clprj_t () : divisor(1), modulus(-1) {}
    clprj_t (cl_t d, cl_t m) : divisor (d), modulus (m) {}
  };

  // apply projection to value
  inline cl_t apply (clprj_t prj, cl_t v) {
    return v / prj.divisor % prj.modulus;
  }

The most important bit, however is this:

.. code-block:: cpp

  // reverse compose projections left \odot right
  inline clprj_t rcompose (clprj_t left, clprj_t right) {
    return clprj_t (left.divisor * right.divisor, right.modulus);
  }

Composing projections is how we get at components of nested
tuples. Its most important that the composite of two projections
is a projection, and the representation above satisfies that
condition.

Pointers
^^^^^^^^

As you know by now, by combining pointers with projection
functions, we obtain a purely functional, referentially
transparent mechanism for address calculations.

So you may wonder how we can get a pointer into a compact
linear product since the value hidden is inside an integer
and is not addressable.

The answer is seen by the C++ representation again:

.. code-block:: cpp

  struct RTL_EXTERN clptr_t 
  {
    cl_t *p;
    cl_t divisor;
    cl_t modulus;
    clptr_t () : p(0), divisor(1),modulus(-1) {}
    clptr_t (cl_t *_p, cl_t d, cl_t m) : p(_p), divisor(d),modulus(m) {}

    // upgrade from ordinary pointer
    clptr_t (cl_t *_p, cl_t siz) : p (_p), divisor(1), modulus(siz) {}
  };

As you can see, a compact linear pointer uses three machine words.
The first word `p` is just a pointer to the whole containing
location, which is a machine word. But we also store a divisor
and modulus value, which identifies how to find the component.

Here's how we get a value using the pointer:

.. code-block:: cpp

  // dereference
  inline cl_t deref(clptr_t q) { return *q.p / q.divisor % q.modulus; }

To apply a projection to a pointer:

.. code-block:: cpp

  // apply projection to pointer
  inline clptr_t applyprj (clptr_t cp, clprj_t d)  {
    return  clptr_t (cp.p, d.divisor * cp.divisor, d.modulus);
  }

And more complicated to store a value in a component:

.. code-block:: cpp

  // storeat
  inline void storeat (clptr_t q, cl_t v) {
      *q.p = *q.p - (*q.p / q.divisor % q.modulus) * q.divisor + v * q.divisor;
      //*q.p -= ((*q.p / q.divisor % q.modulus) - v) * q.divisor; //???
  }
   
Here's an example in Felix, which translates to code using
the C++ above (which is part of the Felix RTL):

.. code-block:: felix

  var x = true,false,true;
  var px = &x;     // ordinary pointer
  var p1 = px . 1; // compact linear pointer
  p1 <- true;      // store 1 bit


Compact linear pointers have read-only and write-only variants too,
which are supertypes of the read-write pointer, the same
as for ordinary pointers.

Pointer type syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: felix

  satom := "_pclt<" stypeexpr "," stypeexpr ">"
  satom := "_rpclt<" stypeexpr "," stypeexpr ">"
  satom := "_wpclt<" stypeexpr "," stypeexpr ">"

A pointer to a compact linear type `_pclt<D,C>` specifies a pointer to a component 
type `C` embedded in a complete compact linear type `D`, which occupies a machine word.
This type is a subtype of the read-only pointer type `_rpclt<D,C>` and
write only pointer type `_wpclt<D,C>`.

