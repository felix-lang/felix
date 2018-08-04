Synchronous Channel Representation
==================================

.. code-block:: cpp

  // ********************************************************
  /// SCHANNEL. Synchronous channels
  // ********************************************************

  struct RTL_EXTERN schannel_t
  {
    slist_t *waiting_to_read;             ///< fthreads waiting for a writer
    slist_t *waiting_to_write;            ///< fthreads waiting for a reader
    schannel_t(gc::generic::gc_profile_t*);
    void push_reader(fthread_t *);        ///< add a reader
    fthread_t *pop_reader();              ///< pop a reader, NULL if none
    void push_writer(fthread_t *);        ///< add a writer
    fthread_t *pop_writer();              ///< pop a writer, NULL if none
  private: // uncopyable
    schannel_t(schannel_t const&) = delete;
    void operator= (schannel_t const&) = delete;
  };


