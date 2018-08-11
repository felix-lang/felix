.. _compactlineartypes:

Compact Linear Types
--------------------

Let 

.. math::

  c_0, c_1, ..., c_{n-1}

be finite sequence of positive integers called *radices*, and let

.. math::

  v_0, v_1, ..., v_{n-1}

be a sequence of non-negative integers called *coefficients* such that

.. math::

  v_i < c_i\ {\rm for\ } i \in 0..n-1

Let

.. math::

   z_0 = 1
   z_i = c_iz_{i-1}=\Prod_{j=0}^{i-1}c_j {\rm for\ } i \in 1..n-1

so that :math:`z_i` is the product of all the :math:`c_j` for 
:math:`j<i`; these quantites are called *weights*.

Let 

.. math::

  a = \Sum_{i=0}^{n-1} v_iz_i

Then the total number of values of is :math:`z_{n-1}` and
the maximum is therefore :math:`z_{n-1}-1`. 

This is known as a *variadic positional number system*.
The for any number in the range 0 to the maximum, 
the coefficients :math:`v_i` are uniquely determined
and there is a bijection between the integer values and the
sequence of coefficients.

Given a value :math:`a` we would like to be able to calculate
the value :math:`v_i` for a given :math:`i`.  Let us first 
rewrite the formula for :math:`a` like this:

.. math::

  a = \Sum_{j=0}^{i-1} v_jz_j + v_iz_i \Sum_{k=i+1}^{n-1} v_kz_k

Now, we perform integer division:

.. math::

  a = {\Sum_{j=0}^{i-1} v_jz_j \over z_i} + v_i + {\Sum_{k=i+1}^{n-1} v_kz_k \over z_i}

Clearly the leftmost term must be zero. We now note :math:`c_i=z_{i+1}/z_i` divides the
right most term exactly,  so the remainder
after a division must be :math:`v_i` as required. In other words:

.. math::

  v_i = (a / z_i) % c_i

An alternate formula applies the remainder operation first then divides:

.. math::

  v_i = (a % z_i ) / z_{i-1}

However, we are not finished. Suppose this :math:`v_i` is itself
composed in variadic positional form with radicies:

.. math::

  c_0', c_1', ..., c_{m-1}'

and coefficients

.. math::

  v_0', v_1', ..., v_{m-1}'

so that now we can find

.. math::

  v_j' = (v_i / z_j') % c_j'

where the :math:`z_j'` are the weights corresponding to the 
:math:`c_k'`. We claim that

.. math::
 
  v_j' = (a / (z_iz_j')) % c_j'


What is important here is that the extractor for a nested coefficient
uses the same formula as the outer one, and in particular
requires the specification of exactly two integers: one to 
do integer division with and one to find the remainder.


Pointer syntax
^^^^^^^^^^^^^^

.. code-block:: felix

  satom := "_pclt<" stypeexpr "," stypeexpr ">"
  satom := "_rpclt<" stypeexpr "," stypeexpr ">"
  satom := "_wpclt<" stypeexpr "," stypeexpr ">"

A pointer to a compact linear type `_pclt<D,C>` specifies a pointer to a component 
type `C` embedded in a complete compact linear type `D`, which occupies a machine word.
This type is a subtype of the read-only pointer type `_rpclt<D,C>` and
write only pointer type `_wpclt<D,C>`.

Projections
^^^^^^^^^^^

Projections for components of compact linear products use the same syntax
as for non-compact products, as do the overloads for pointers. In order
to get a compact linear pointer we can first store a complete compact
linear value in a variable. Then starting with an ordinary machine
pointer to the variable, a pointer projection to a component will
yield a compact linear pointer. For example:

.. code-block:: felix

  var x = true,false,true;
  var px = &x;     // ordinary pointer
  var p1 = px . 1; // compact linear pointer
  p1 <- true;      // store 1 bit


