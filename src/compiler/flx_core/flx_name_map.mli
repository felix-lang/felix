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

val mkentry:
  bid_t ref ->
  Flx_types.ivs_list_t ->
  bid_t ->
  entry_kind_t

val review_entry: 
  bid_t ref ->
  string ->
  Flx_srcref.t ->
  Flx_types.bvs_t ->
  Flx_btype.t list ->
  entry_kind_t ->
  entry_kind_t


val review_entry_set:
  bid_t ref ->
  string -> 
  entry_set_t ->
  Flx_srcref.t ->
  Flx_types.bvs_t ->
  Flx_btype.t list ->
  entry_set_t






