(** Why interface
 *
 * Interface to Why program. *)

open Flx_ast
open Flx_types
open Flx_typing
open Flx_mtypes2

val emit_whycode:
  string ->
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int -> (* root for lookup of and/or/not etc *)
  unit
