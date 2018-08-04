Jump Address Representation
===========================

.. code-block:: cpp

  struct RTL_EXTERN jump_address_t
  {
    con_t *target_frame;
    FLX_LOCAL_LABEL_VARIABLE_TYPE local_pc;

    jump_address_t (con_t *tf, FLX_LOCAL_LABEL_VARIABLE_TYPE lpc) : 
      target_frame (tf), local_pc (lpc) 
    {}
    jump_address_t () : target_frame (0), local_pc(0) {}
    jump_address_t (con_t *tf) : target_frame(tf), local_pc(0) {}
    // default copy constructor and assignment
  };


