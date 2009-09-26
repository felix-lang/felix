(** Monomorphism
 *
 * This requires the instantiator to have been run. *)

val monomorphise:
  Flx_mtypes2.sym_state_t ->
  Flx_types.bsym_table_t ->
  unit
