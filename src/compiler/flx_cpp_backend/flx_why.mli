(** Why interface
 *
 * Interface to Why program. *)

open Flx_types
open Flx_mtypes2

val emit_whycode:
  string ->
  sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  bid_t -> (* root for lookup of and/or/not etc *)
  unit
