(** Display calcs *)

open Flx_types
open Flx_mtypes2
open Flx_bid

val get_display_list:
  Flx_bsym_table.t ->
  bid_t ->
  (bid_t * int) list

val cal_display:
  Flx_bsym_table.t ->
  bid_t option ->
  (bid_t * int) list
