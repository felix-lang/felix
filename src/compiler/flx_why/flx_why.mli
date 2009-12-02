(** Why interface
 *
 * Interface to Why program. *)

val emit_whycode:
  string ->
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t -> (* root for lookup of and/or/not etc *)
  unit
