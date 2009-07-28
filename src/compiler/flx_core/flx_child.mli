open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

type child_map_t =
  (bid_t, bid_t list) Hashtbl.t

val find_children:
  child_map_t -> bid_t -> bid_t list

val is_child:
  child_map_t -> bid_t -> bid_t -> bool

val add_child:
  child_map_t -> bid_t -> bid_t -> unit

val remove_child:
  child_map_t -> bid_t -> bid_t -> unit

val is_ancestor:
  fully_bound_symbol_table_t -> bid_t -> bid_t -> bool

val descendants:
  child_map_t -> bid_t -> IntSet.t

val cal_children:
  fully_bound_symbol_table_t -> child_map_t
