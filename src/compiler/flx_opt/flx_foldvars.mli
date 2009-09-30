(** Fold vars *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call

val fold_vars:
  sym_state_t ->
  bsym_table_t ->
  Flx_child.t ->
  usage_table_t ->
  int ->
  bparameter_t list ->
  bexe_t list ->
  bexe_t list

val add_use:
  usage_table_t -> int -> int -> Flx_srcref.t -> unit
