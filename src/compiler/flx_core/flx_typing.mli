open Flx_ast
open Flx_btype

exception UnificationError of Flx_btype.t * Flx_btype.t

val flx_unit: typecode_t
val flx_bool : typecode_t
val flx_bbool : Flx_btype.t

val type_of_argtypes :
  typecode_t list ->
  typecode_t

val funparamtype : 'a * 'b * 't * 'd -> 't

module FuntypeSet : Set.S with type elt = typecode_t

module FunInstSet : Set.S with type elt = Flx_types.bid_t * Flx_btype.t list

val sye: entry_kind_t -> Flx_types.bid_t

val mktypefun:
  Flx_srcref.t ->
  Flx_id.t ->
  vs_list_t ->
  (Flx_id.t * typecode_t) list list ->
  typecode_t ->
  typecode_t ->
  statement_t
