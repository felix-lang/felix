(** Uncurry generation *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call

val uncurry_gen:
  sym_state_t ->
  (bid_t, bid_t list) Hashtbl.t *
  fully_bound_symbol_table_t ->
  int (* number of new functions made *)
