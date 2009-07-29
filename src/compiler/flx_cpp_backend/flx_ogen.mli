(** GC shape object generator *)

open Flx_ast
open Flx_types
open Flx_mtypes2

val gen_offset_tables:
  sym_state_t ->
  (bid_t, bid_t list) Hashtbl.t *
  fully_bound_symbol_table_t ->
  string ->
  string

val find_thread_vars_with_type:
  fully_bound_symbol_table_t ->
  (bid_t * btypecode_t) list

val find_references:
  sym_state_t ->
  (bid_t, bid_t list) Hashtbl.t *
  fully_bound_symbol_table_t ->
  bid_t ->
  btypecode_t list ->
  (bid_t * btypecode_t) list
