(* Beta reduction *)

val beta_reduce:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_srcref.t ->
  Flx_btype.t ->
  Flx_btype.t
