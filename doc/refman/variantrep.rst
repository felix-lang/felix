General Variant Representation
==============================

.. code-block:: cpp

  // ********************************************************
  /// VARIANTS. Felix union type
  /// note: non-polymorphic, so ctor can be inline
  // ********************************************************

  struct RTL_EXTERN _uctor_
  {
    int variant;  ///< Variant code
    void *data;   ///< Heap variant constructor data
    _uctor_() : variant(-1), data(0) {}
    _uctor_(int i, void *d) : variant(i), data(d) {}
    _uctor_(int *a, _uctor_ x) : variant(a[x.variant]), data(x.data) {}
  };


