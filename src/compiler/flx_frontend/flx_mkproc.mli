(** mkproc *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

val mkproc_gen:
  sym_state_t ->
  (bid_t, bid_t list) Hashtbl.t *
  fully_bound_symbol_table_t ->
  int
