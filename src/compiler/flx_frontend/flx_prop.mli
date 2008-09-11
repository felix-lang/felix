(** Elide unused entries
 *
 * Name binding pass 2. *)

open Flx_ast
open Flx_types
open Flx_typing
open Flx_set
open Flx_mtypes2

val add_prop:
  fully_bound_symbol_table_t  ->
  property_t ->
  int ->
  unit

val rem_prop:
  fully_bound_symbol_table_t  ->
  property_t ->
  int ->
  unit

val get_vs:
  fully_bound_symbol_table_t  ->
  int ->
  bvs_t
