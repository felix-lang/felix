(** GC shape object generator *)

val gen_offset_tables:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  string ->
  string

val find_thread_vars_with_type:
  Flx_bsym_table.t ->
  (Flx_types.bid_t * Flx_btype.t) list

val find_references:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  Flx_btype.t list ->
  (Flx_types.bid_t * Flx_btype.t) list
