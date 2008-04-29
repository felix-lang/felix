(** Label management *)

open Flx_ast
open Flx_types
open Flx_mtypes2

type label_map_t =
  (bid_t,(string, int) Hashtbl.t) Hashtbl.t

val create_label_map:
  fully_bound_symbol_table_t ->
  int ref ->
  label_map_t

type goto_kind_t =
[
  | `Local of int          (* index *)
  | `Nonlocal of int * int (* index, parent *)
  | `Unreachable
]

val find_label:
  fully_bound_symbol_table_t ->
  label_map_t ->
  int ->
  string ->
  goto_kind_t

type label_kind_t = [`Far | `Near | `Unused]

type label_usage_t = (int,label_kind_t) Hashtbl.t

val create_label_usage:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  label_map_t ->
  label_usage_t

val get_label_kind:
  label_map_t ->
  label_usage_t ->
  bid_t -> (* container *)
  string -> (* label *)
  label_kind_t

val get_label_kind_from_index:
  label_usage_t ->
  int ->
  label_kind_t
