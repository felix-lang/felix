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

val is_ancestor: Flx_bsym_table.t -> bid_t -> bid_t -> bool

val descendants: t -> bid_t -> BidSet.t

val cal_children: Flx_bsym_table.t -> t
