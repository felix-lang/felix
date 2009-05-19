(** Overload resolution *)

open Flx_ast
open Flx_types
open Flx_mtypes2

type overload_result =
 int *  (* index of function *)
 btypecode_t * (* type of function signature *)
 btypecode_t * (* type of function return *)
 (int * btypecode_t) list * (* mgu *)
 btypecode_t list (* ts *)

val overload:
  sym_state_t ->
  (Flx_srcref.t  -> int -> typecode_t -> btypecode_t) -> (* bind type *)
  (int -> expr_t -> tbexpr_t) -> (* bind expression in context of i *)
  (int -> qualified_name_t -> entry_set_t * typecode_t list) ->
  Flx_srcref.t ->
  entry_kind_t list ->
  string ->
  btypecode_t list ->
  btypecode_t list ->
  overload_result option

exception OverloadKindError of Flx_srcref.t * string
