(** Make stack calls
 *
 * Name binding pass 2 *)

open Flx_ast
open Flx_types
open Flx_mtypes2
open Flx_child
open Flx_label

val make_stack_calls:
  sym_state_t ->
  child_map_t * fully_bound_symbol_table_t ->
  label_map_t -> label_usage_t ->
  unit

val can_stack_func:
  (btypecode_t, [`Recurse | `Safe | `Unsafe] ) Hashtbl.t ->
  sym_state_t ->
  child_map_t * fully_bound_symbol_table_t ->
  int ->
  bool
