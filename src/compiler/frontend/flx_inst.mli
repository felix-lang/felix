(** Elide unused entries
 *
 * Name binding pass 2 *)

open Flx_ast
open Flx_types
open Flx_typing
open Flx_mtypes2

val instantiate:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  bool -> (* instantiate parameters? *)
  bid_t ->
  biface_t list ->
  unit
