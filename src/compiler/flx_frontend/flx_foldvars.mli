(** Fold vars *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call
open Flx_child

val fold_vars:
  sym_state_t ->
  usage_table_t * child_map_t * fully_bound_symbol_table_t ->
  int ->
  bparameter_t list ->
  bexe_t list ->
  bexe_t list

val add_use:
  usage_table_t -> int -> int -> Flx_srcref.t -> unit
