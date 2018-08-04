Continuation Base
=================

.. code-block:: cpp

  struct FLX_EXCEPTIONS_EXTERN con_t ///< abstract base for mutable continuations
  {
    FLX_PC_DECL               ///< interior program counter
    struct _uctor_ *p_svc;           ///< pointer to service request

    con_t();                  ///< initialise pc, p_svc to 0
    virtual con_t *resume()=0;///< method to perform a computational step
    virtual ~con_t();
    con_t * _caller;          ///< callers continuation (return address)
  };

