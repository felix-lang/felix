(** GC shape object generator *)

val gen_offset_tables:
  Flx_mtypes2.sym_state_t ->
  Flx_types.bsym_table_t ->
  Flx_child.t ->
  string ->
  string

val find_thread_vars_with_type:
  Flx_types.bsym_table_t ->
  (Flx_types.bid_t * Flx_types.btypecode_t) list

val find_references:
  Flx_mtypes2.sym_state_t ->
  Flx_types.bsym_table_t ->
  Flx_child.t ->
  Flx_types.bid_t ->
  Flx_types.btypecode_t list ->
  (Flx_types.bid_t * Flx_types.btypecode_t) list
