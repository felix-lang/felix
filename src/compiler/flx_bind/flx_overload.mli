(** Overload resolution *)

open Flx_ast
open Flx_types
open Flx_mtypes2

type overload_result =
 bid_t *  (* index of function *)
 btypecode_t * (* type of function signature *)
 btypecode_t * (* type of function return *)
 (bid_t * btypecode_t) list * (* mgu *)
 btypecode_t list (* ts *)

val overload:
  sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  env_t ->
  recstop ->
  (recstop -> Flx_srcref.t -> bid_t -> typecode_t -> btypecode_t) -> (* bind type *)
  (bid_t -> expr_t -> Flx_bexpr.t) -> (* bind expression in context of i *)
  (bid_t -> qualified_name_t -> entry_set_t * typecode_t list) ->
  Flx_srcref.t ->
  entry_kind_t list ->
  string ->
  btypecode_t list ->
  btypecode_t list ->
  overload_result option

exception OverloadKindError of Flx_srcref.t * string
