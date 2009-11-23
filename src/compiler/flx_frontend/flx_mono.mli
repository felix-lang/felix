(** Monomorphism
 *
 * This requires the instantiator to have been run. *)

val monomorphise:
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  unit
