(** Mappings *)

open Flx_ast
open Flx_types

val map_type:
  (typecode_t -> typecode_t) -> typecode_t -> typecode_t

val map_expr:
  (expr_t -> expr_t) ->
  expr_t ->
  expr_t

(** Simplify the bound expression. *)
val reduce_tbexpr:
  Flx_bexpr.t -> (** The bound expression. *)
  Flx_bexpr.t

(** Simplify the bound exe. *)
val reduce_bexe:
  Flx_bexe.t -> (** The bound exe. *)
  Flx_bexe.t

val scan_expr: expr_t -> Flx_srcref.t list
