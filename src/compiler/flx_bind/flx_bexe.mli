(** Bind executable statements *)

open Flx_ast
open Flx_types
open Flx_mtypes2

val bind_exes:
  sym_state_t ->
  env_t ->
  Flx_srcref.t ->
  (Flx_srcref.t * exe_t) list ->
  btypecode_t ->
  string ->
  bid_t ->
  bvs_t ->
  btypecode_t * bexe_t list
