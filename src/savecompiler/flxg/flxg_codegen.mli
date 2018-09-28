(** Generate the C++ code. *)
open Flx_bid

val codegen:
  Flxg_state.t ->
  Flx_bsym_table.t ->
  bid_t option ->
  unit
