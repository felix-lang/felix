(** Generate the C++ code. *)
val codegen:
  Flxg_state.t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t option ->
  unit
