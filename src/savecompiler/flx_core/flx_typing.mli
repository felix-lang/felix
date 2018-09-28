open Flx_ast
open Flx_btype
open Flx_bid

exception UnificationError of Flx_btype.t * Flx_btype.t

val flx_unit: typecode_t
val flx_bool : typecode_t
val flx_bbool : Flx_btype.t

val type_of_argtypes :
  typecode_t list ->
  typecode_t

val funparamtype : 'a * 'b * 't * 'd -> 't

module FuntypeSet : Set.S with type elt = typecode_t

module FunInstSet : Set.S with type elt = bid_t * Flx_btype.t list

val sye: Flx_name_map.entry_kind_t -> bid_t

val mktypefun:
  Flx_srcref.t ->
  Flx_id.t ->
  vs_list_t ->
  (Flx_id.t * kindcode_t) list list ->
  kindcode_t ->
  typecode_t ->
  statement_t
