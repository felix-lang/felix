(** Name binding
 *
 * Name binding pass 2 *)

open Flx_types
open Flx_ast
open Flx_mtypes2

val bbind:
  sym_state_t ->
  fully_bound_symbol_table_t

val bind_ifaces:
  sym_state_t ->
  (Flx_srcref.t * iface_t * int option) list ->
  biface_t list
