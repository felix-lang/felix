(** Elide unused entries
 *
 * Name binding pass 2 *)
open Flx_bid
val instantiate:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  bool -> (* instantiate parameters? *)
  bid_t option ->
  Flx_btype.biface_t list ->
  unit
