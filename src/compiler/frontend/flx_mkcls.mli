(** Elide unused entries
 *
 * Name binding pass 2. *)

open Flx_ast
open Flx_types
open Flx_typing
open Flx_set
open Flx_mtypes2

val make_closures:
  sym_state_t ->
  fully_bound_symbol_table_t  -> unit
