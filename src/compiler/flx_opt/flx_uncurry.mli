(** Uncurry generation *)

val uncurry_gen:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  int (* number of new functions made *)
