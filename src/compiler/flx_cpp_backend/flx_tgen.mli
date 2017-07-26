(** Type generator *)
open Flx_bid
open Flx_types
open Flx_mtypes2

val gen_types :
  sym_state_t ->
  Flx_bsym_table.t ->
  (bid_t * Flx_btype.t) list -> string

val gen_type_names :
  sym_state_t ->
  Flx_bsym_table.t ->
  (bid_t * Flx_btype.t) list -> string
