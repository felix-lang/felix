(** Expression unraveller *)

val unravel:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_types.tbexpr_t ->
  (Flx_types.tbexpr_t * string) list * Flx_types.tbexpr_t
