(* Beta reduction *)

val beta_reduce:
  Flx_types.bid_t ref ->
  Flx_bsym_table.t ->
  Flx_srcref.t ->
  Flx_btype.t ->
  Flx_btype.t
