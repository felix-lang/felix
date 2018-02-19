(** Fold vars *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call
open Flx_bid

val fold_vars:
  sym_state_t ->
  Flx_bsym_table.t ->
  usage_table_t ->
  bid_t ->
  Flx_bparams.t ->
  Flx_bexe.t list ->
  Flx_bexe.t list

val add_use:
  usage_table_t -> bid_t -> bid_t -> Flx_srcref.t -> unit
