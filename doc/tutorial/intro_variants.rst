Variants
========

Variants are dual to structs. Whereas structs represent "this and that and the other",
variants represent "this or that or the other", in other words, alternatives.

Definition
++++++++++

We have already met a common variant, the option type:

.. code-block:: felix

  variant opt[t] =
  | None
  | Some of t
  ;

The names `None` and `Some` are sometimes called *type constructors*. When they're
used to make a value of a variant type, they're functions, known as *injection
functions* because they inject their argument into the variant type.

Construction
++++++++++++

Here's how we can make values of the option type:

.. code-block:: felix

  var n = None[int];
  var s = Some 42;

Notice that for the `None` case we have to provide the instance type of `t`,
in this case `int`. For the `Some` case, the instance `int` of `t` is deduced
by overload resolution and so can be elided.

Extraction
++++++++++

Injections are conditionally invertible, that is, the inverses are
partial functions which I call *extractors*. You use these with pattern matches:

.. code-block:: felix

   fun show (x:opt[int]) =>
     match x with
     | None => "None"
     | Some i => "Some(" + i.str + ")"
   ;

Felix "knows" which injection was used to construct a variant value,
and selects the matching branch from the `match` expression.
This ensures the extractor is well defined. For the `Some` branch,
the extract sets `i` to the argument of the injection function used
to construct the value.



