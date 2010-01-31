(** Mappings *)

open Flx_ast
open Flx_types

val map_type:
  (typecode_t -> typecode_t) -> typecode_t -> typecode_t

val map_expr:
  (expr_t -> expr_t) ->
  expr_t ->
  expr_t

val scan_expr: expr_t -> Flx_srcref.t list
