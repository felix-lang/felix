(** Fold vars *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call

val fold_vars:
  sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_child.t ->
  usage_table_t ->
  bid_t ->
  bparameter_t list ->
  bexe_t list ->
  bexe_t list

val add_use:
  usage_table_t -> bid_t -> bid_t -> Flx_srcref.t -> unit
