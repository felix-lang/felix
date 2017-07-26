open Flx_bid
type entry_kind_t = {
  base_sym : bid_t;
  spec_vs : (string * bid_t) list;
  sub_ts : Flx_btype.t list;
}
type entry_set_t =
    FunctionEntry of entry_kind_t list
  | NonFunctionEntry of entry_kind_t
type name_map_t = (string, entry_set_t) Hashtbl.t

val map_entry :
  (bid_t -> bid_t) ->
  (Flx_btype.t -> Flx_btype.t) -> entry_kind_t -> entry_kind_t
val map_name_map :
  (bid_t -> bid_t) ->
  (Flx_btype.t -> Flx_btype.t) -> ('a, entry_set_t) Hashtbl.t -> ('a, entry_set_t) Hashtbl.t
