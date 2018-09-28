(** Arg fidler *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_bid
(*
val unpack:
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_bparams.xps_t list ->
  Flx_bexpr.t ->
  Flx_bexpr.t list
*)

val merge_args:
  sym_state_t ->
  Flx_bsym_table.t ->
  bid_t -> bid_t -> Flx_bexpr.t -> Flx_bexpr.t ->
  Flx_bexpr.t

val append_args:
  sym_state_t ->
  Flx_bsym_table.t ->
  bid_t -> Flx_bexpr.t -> Flx_bexpr.t list ->
  Flx_bexpr.t
