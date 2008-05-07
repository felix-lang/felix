(** Control flow *)

open Flx_types
open Flx_mtypes2

val tailable:
  bexe_t list ->
  string list ->
  bexe_t list ->
  bool

val chain_gotos:
  sym_state_t ->
  bexe_t list -> bexe_t list

val final_tailcall_opt:
  bexe_t list -> bexe_t list
