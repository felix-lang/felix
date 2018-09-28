(** Expression unraveller *)

val unravel:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_bexpr.t ->
  (Flx_bexpr.t * string) list * Flx_bexpr.t
