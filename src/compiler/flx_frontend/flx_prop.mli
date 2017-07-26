(** Elide unused entries
 *
 * Name binding pass 2. *)
open Flx_bid
val add_prop:
  Flx_bsym_table.t ->
  Flx_ast.property_t ->
  bid_t ->
  unit

val rem_prop:
  Flx_bsym_table.t ->
  Flx_ast.property_t ->
  bid_t ->
  unit
