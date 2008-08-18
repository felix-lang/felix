(* Meta typing and beta reduction *)

open Flx_types
open Flx_mtypes2
open Flx_ast

val metatype:
  sym_state_t ->
  range_srcref ->
  btypecode_t ->
  btypecode_t

val beta_reduce:
  sym_state_t ->
  range_srcref ->
  btypecode_t ->
  btypecode_t
