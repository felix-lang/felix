open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

type t = (bid_t, bid_t list) Hashtbl.t

val make: unit -> t

val find_children: t -> bid_t -> bid_t list

val is_child: t -> bid_t -> bid_t -> bool

val add_child: t -> bid_t -> bid_t -> unit

val remove_child: t -> bid_t -> bid_t -> unit

val is_ancestor: bsym_table_t -> bid_t -> bid_t -> bool

val descendants: t -> bid_t -> IntSet.t

val cal_children: bsym_table_t -> t
