(** C++ code generator *)

open Flx_ast
open Flx_types
open Flx_mtypes2
open Flx_ctypes

val gen_prim_call :
  sym_state_t ->
  fully_bound_symbol_table_t ->
  (btypecode_t -> btypecode_t) ->
  (Flx_srcref.t -> tbexpr_t -> cexpr_t) ->
  string ->
  btypecode_t list ->
  tbexpr_t ->
  string ->
  Flx_srcref.t ->
  Flx_srcref.t ->
  string ->
  cexpr_t

val shape_of:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  (btypecode_t -> string) ->
  btypecode_t ->
  string
