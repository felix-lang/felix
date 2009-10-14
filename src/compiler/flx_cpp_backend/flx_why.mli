(** Why interface
 *
 * Interface to Why program. *)

open Flx_types
open Flx_mtypes2

val emit_whycode:
  string ->
  sym_state_t ->
  bsym_table_t ->
  bid_t -> (* root for lookup of and/or/not etc *)
  unit
