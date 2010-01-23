(** Control flow *)

open Flx_types
open Flx_mtypes2

val tailable:
  Flx_bexe.t list ->
  string list ->
  Flx_bexe.t list ->
  bool

val chain_gotos:
  sym_state_t ->
  Flx_bexe.t list -> Flx_bexe.t list

val final_tailcall_opt:
  Flx_bexe.t list -> Flx_bexe.t list
