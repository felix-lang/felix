open Flx_ast
open Flx_types
exception UnificationError of btypecode_t * btypecode_t
val flx_bool : typecode_t
val flx_bbool : btypecode_t

val is_unitsum: btypecode_t -> bool
val int_of_unitsum : btypecode_t -> int
val all_units0 : b0typecode_t list -> bool
val all_units : btypecode_t list -> bool
val all_voids : btypecode_t list -> bool

val cmp_literal: literal_t -> literal_t -> bool
val cmp_tbexpr: tbexpr_t -> tbexpr_t -> bool

val type_of_argtypes :
  typecode_t list ->
  typecode_t

val funparamtype : 'a * 'b * 't * 'd -> 't

val typeoflist:
  btypecode_t list ->
  btypecode_t

val typeofbps_traint: bparams_t -> btypecode_t list
val typeofbps: bparameter_t list-> btypecode_t list

module FuntypeSet : Set.S with type elt = typecode_t

module FunInstSet : Set.S with type elt = bid_t * btypecode_t list

val sye: entry_kind_t -> int

val mktypefun:
  range_srcref ->
  string ->
  vs_list_t ->
  (string * typecode_t) list list ->
  typecode_t ->
  typecode_t ->
  statement_t
