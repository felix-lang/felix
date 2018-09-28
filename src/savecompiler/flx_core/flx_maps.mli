(** Mappings *)

open Flx_ast
open Flx_types

val map_type:
  (typecode_t -> typecode_t) -> typecode_t -> typecode_t

val map_expr:
  (expr_t -> expr_t) ->
  expr_t ->
  expr_t

val full_map_expr:
  (Flx_bid.bid_t -> Flx_bid.bid_t) -> (* index map *)
  (typecode_t -> typecode_t) -> (* type map *)
  (expr_t -> expr_t) -> (* expr map *)
  expr_t -> expr_t

val map_exe: 
  (Flx_bid.bid_t -> Flx_bid.bid_t) -> (* index map *)
  (typecode_t -> typecode_t) -> (* type map *)
  (expr_t -> expr_t) -> (* expr map *)
  exe_t -> exe_t

val iter_exe:
   (Flx_bid.bid_t -> unit) ->
   (typecode_t -> unit) ->
   (expr_t -> unit) -> 
   exe_t ->
   unit

val scan_expr: expr_t -> Flx_srcref.t list
