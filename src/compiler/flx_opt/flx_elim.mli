type elim_state_t

val make_elim_state:
  Flx_mtypes2.sym_state_t ->
  Flx_types.bsym_table_t ->
  elim_state_t

(** Eliminate unused values and variables. *)
val eliminate_unused: elim_state_t -> unit
