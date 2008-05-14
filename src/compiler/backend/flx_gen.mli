(** C++ code generator *)

open Flx_ast
open Flx_types
open Flx_mtypes2
open Flx_label

val gen_function_names:
  sym_state_t ->
  (bid_t, bid_t list) Hashtbl.t *
  fully_bound_symbol_table_t ->
  string

val gen_functions:
  sym_state_t ->
  (bid_t, bid_t list) Hashtbl.t *
  fully_bound_symbol_table_t ->
  string

val gen_execute_methods:
  string ->
  sym_state_t ->
  (bid_t, bid_t list) Hashtbl.t *
  fully_bound_symbol_table_t ->
  label_map_t * label_usage_t ->
  int ref ->
  out_channel ->
  out_channel ->
  unit

val find_members:
  sym_state_t ->
  (bid_t, bid_t list) Hashtbl.t *
  fully_bound_symbol_table_t ->
  int ->
  btypecode_t list ->
  string

val gen_biface_headers:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  biface_t list ->
  string

val gen_biface_bodies:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  biface_t list ->
  string

val format_vars:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  bid_t list ->
  btypecode_t list ->
  string

val is_gc_pointer:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  range_srcref ->
  btypecode_t ->
  bool
