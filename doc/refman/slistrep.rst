Slist Representation
====================

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




