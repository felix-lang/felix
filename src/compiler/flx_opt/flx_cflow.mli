(** Control flow *)

open Flx_types
open Flx_mtypes2
open Flx_bid

val tailable:
  Flx_bexe.t list ->
  bid_t list ->
  Flx_bexe.t list ->
  bool

val chain_gotos:
  sym_state_t ->
  string -> (* name of func *)
  Flx_btype.t -> (* return type to indicate if proc or fun *)
  Flx_bexe.t list -> Flx_bexe.t list

val final_tailcall_opt:
  Flx_bexe.t list -> Flx_bexe.t list
