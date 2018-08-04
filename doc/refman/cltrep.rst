Compact Linear Type Representation
==================================

A complete value of a compact linear type is stored in a 64 bit machine word.


.. code-block:: cpp

  typedef ::std::uint64_t cl_t; 

  // ********************************************************
  /// COMPACT LINEAR PROJECTIONS 
  // ********************************************************

  struct RTL_EXTERN clprj_t 
  {
    cl_t divisor;
    cl_t modulus;
    clprj_t () : divisor(1), modulus(-1) {}
    clprj_t (cl_t d, cl_t m) : divisor (d), modulus (m) {}

  };

  // reverse compose projections left \odot right
  inline clprj_t rcompose (clprj_t left, clprj_t right) {
    return clprj_t (left.divisor * right.divisor, right.modulus);
  }

  // apply projection to value
  inline cl_t apply (clprj_t prj, cl_t v) {
    return v / prj.divisor % prj.modulus;
  }

  // ********************************************************
  /// COMPACT LINEAR POINTERS
  // ********************************************************

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

  // apply projection to pointer
  inline clptr_t applyprj (clptr_t cp, clprj_t d)  {
    return  clptr_t (cp.p, d.divisor * cp.divisor, d.modulus);
  }

  // dereference
  inline cl_t deref(clptr_t q) { return *q.p / q.divisor % q.modulus; }

  // storeat
  inline void storeat (clptr_t q, cl_t v) {
      *q.p = *q.p - (*q.p / q.divisor % q.modulus) * q.divisor + v * q.divisor;
      //*q.p -= ((*q.p / q.divisor % q.modulus) - v) * q.divisor; //???
  }


