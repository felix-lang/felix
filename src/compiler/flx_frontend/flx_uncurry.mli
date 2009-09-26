(** Uncurry generation *)

val uncurry_gen:
  Flx_mtypes2.sym_state_t ->
  Flx_types.bsym_table_t ->
  Flx_child.t ->
  int (* number of new functions made *)
