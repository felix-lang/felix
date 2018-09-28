(** Mark closures for heap. *)

val mark_heap_closures :
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  unit
