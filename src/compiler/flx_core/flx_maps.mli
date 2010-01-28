(** Mappings *)

open Flx_ast
open Flx_types

val map_type:
  (typecode_t -> typecode_t) -> typecode_t -> typecode_t

val map_btype:
  (btypecode_t -> btypecode_t) -> btypecode_t -> btypecode_t

val iter_btype:
  (btypecode_t -> unit) -> btypecode_t -> unit

val flat_iter_tbexpr:
  (bid_t -> unit) ->
  (Flx_bexpr.t -> unit) ->
  (btypecode_t -> unit) ->
  Flx_bexpr.t -> unit

val iter_tbexpr:
  (bid_t -> unit) ->
  (Flx_bexpr.t -> unit) ->
  (btypecode_t -> unit) ->
  Flx_bexpr.t -> unit

val map_expr:
  (expr_t -> expr_t) ->
  expr_t ->
  expr_t

val map_tbexpr:
  (bid_t -> bid_t) ->
  (Flx_bexpr.t -> Flx_bexpr.t) ->
  (btypecode_t -> btypecode_t) ->
  Flx_bexpr.t -> Flx_bexpr.t

val iter_bexe:
  (bid_t -> unit) ->
  (Flx_bexpr.t -> unit) ->
  (btypecode_t -> unit) ->
  (string -> unit) ->
  (string -> unit) ->
  Flx_bexe.t -> unit

val map_bexe:
  (bid_t -> bid_t) ->
  (Flx_bexpr.t -> Flx_bexpr.t) ->
  (btypecode_t -> btypecode_t) ->
  (string -> string) ->
  (string -> string) ->
  Flx_bexe.t -> Flx_bexe.t

(** Simplify the bound expression. *)
val reduce_tbexpr:
  Flx_bexpr.t -> (** The bound expression. *)
  Flx_bexpr.t

(** Simplify the bound exe. *)
val reduce_bexe:
  Flx_bexe.t -> (** The bound exe. *)
  Flx_bexe.t

val scan_expr: expr_t -> Flx_srcref.t list
