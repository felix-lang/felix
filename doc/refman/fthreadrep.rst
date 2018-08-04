Fibre Representation
====================

.. code-block:: cpp

  struct RTL_EXTERN fthread_t // fthread abstraction
  {
    con_t *cc;                    ///< current continuation

    fthread_t();                  ///< dead thread, suitable for assignment
    fthread_t(con_t*);            ///< make thread from a continuation
    _uctor_ *run();               ///< run until dead or driver service request
    void kill();                  ///< kill by detaching the continuation
    _uctor_ *get_svc()const;      ///< get current service request of waiting thread
  private: // uncopyable
    fthread_t(fthread_t const&) = delete;
    void operator=(fthread_t const&) = delete;
  };

