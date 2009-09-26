(** Expression unraveller *)

val unravel:
  Flx_mtypes2.sym_state_t ->
  Flx_types.bsym_table_t ->
  Flx_types.tbexpr_t ->
  (Flx_types.tbexpr_t * string) list * Flx_types.tbexpr_t
