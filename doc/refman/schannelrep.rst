Synchronous Channel Representation
==================================

.. code-block:: cpp


  // ********************************************************
  /// SLIST. singly linked lists: SHARABLE and COPYABLE
  /// SLIST manages pointers to memory managed by the collector
  // ********************************************************

  struct RTL_EXTERN slist_node_t {
    slist_node_t *next;
    void *data;
    slist_node_t(slist_node_t *n, void *d) : next(n), data(d) {}
  };


  struct RTL_EXTERN slist_t {
    slist_t(){} // hack
    gc::generic::gc_profile_t *gcp;
    struct slist_node_t *head;

    slist_t (gc::generic::gc_profile_t*); ///< create empty list

    void push(void *data);                ///< push a gc pointer
    void *pop();                          ///< pop a gc pointer
    bool isempty()const;
  };


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


