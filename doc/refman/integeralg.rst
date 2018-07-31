Integer Algebra
===============

Semantics
---------

.. code-block:: felix

  //$ Integers.
  class Integer[t] {
    inherit Tord[t];
    inherit Dring[t];
    inherit Bidirectional[t];
    virtual fun << : t * t -> t = "$1<<$2";
    virtual fun >> : t * t -> t = "$1>>$2";

    fun shl(x:t,y:t)=> x << y;
    fun shr(x:t,y:t)=> x >> y;

    virtual fun maxval: 1 -> t = "::std::numeric_limits<?1>::max()";
    virtual fun minval: 1 -> t = "::std::numeric_limits<?1>::min()";

  }

  //$ Signed Integers.
  class Signed_integer[t] {
    inherit Integer[t];
    virtual fun sgn: t -> int;
    virtual fun abs: t -> t;
  }

  //$ Unsigned Integers.
  class Unsigned_integer[t] {
    inherit Integer[t];
    inherit Bits[t];
  }



