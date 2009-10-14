(** Arg fidler *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

val get_ps:
  bsym_table_t ->
  bid_t ->
  bparameter_t list

val unpack:
  sym_state_t ->
  bsym_table_t ->
  bid_t ->
  bparameter_t list ->
  tbexpr_t ->
  tbexpr_t list

val merge_args:
  sym_state_t ->
  bsym_table_t ->
  bid_t -> bid_t -> tbexpr_t -> tbexpr_t ->
  tbexpr_t

val append_args:
  sym_state_t ->
  bsym_table_t ->
  bid_t -> tbexpr_t -> tbexpr_t list ->
  tbexpr_t
