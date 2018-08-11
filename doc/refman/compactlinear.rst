.. _compactlineartypes:

Compact Linear Types
--------------------

Let 

.. math::

  c_0, c_1, ..., c_{n-1}

be finite sequence of positive integers and let

.. math::

  v_0, v_1, ..., v_{n-1}

be a sequence of non-negative integers such that

.. math::

  v_i < c_i\ {\rm for} i \in 0..n-1

 

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


