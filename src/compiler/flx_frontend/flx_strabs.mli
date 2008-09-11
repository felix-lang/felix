(** Downgrade abstract types
 *
 * Convert newtype abstractions to their representations. *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

val strabs:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  fully_bound_symbol_table_t
