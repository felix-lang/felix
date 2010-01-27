(** Arg fidler *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

val unpack:
  sym_state_t ->
  Flx_bsym_table.t ->
  bid_t ->
  Flx_bparameter.t list ->
  tbexpr_t ->
  tbexpr_t list

val merge_args:
  sym_state_t ->
  Flx_bsym_table.t ->
  bid_t -> bid_t -> tbexpr_t -> tbexpr_t ->
  tbexpr_t

val append_args:
  sym_state_t ->
  Flx_bsym_table.t ->
  bid_t -> tbexpr_t -> tbexpr_t list ->
  tbexpr_t
