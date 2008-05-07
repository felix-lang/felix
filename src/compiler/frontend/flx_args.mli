(** Arg fidler *)

open Flx_ast
open Flx_types
open Flx_mtypes1
open Flx_mtypes2

val get_ps:
  fully_bound_symbol_table_t ->
  int ->
  bparameter_t list

val unpack:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int ->
  bparameter_t list ->
  tbexpr_t ->
  tbexpr_t list

val merge_args:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int -> int -> tbexpr_t -> tbexpr_t ->
  tbexpr_t

val append_args:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int -> tbexpr_t -> tbexpr_t list ->
  tbexpr_t
