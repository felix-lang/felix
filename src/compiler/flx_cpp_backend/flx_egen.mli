val gen_expr:
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  Flx_types.tbexpr_t ->
  Flx_types.bvs_t ->
  Flx_types.btypecode_t list ->
  Flx_srcref.t ->
  string

val gen_expr':
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  Flx_types.tbexpr_t ->
  Flx_types.bvs_t ->
  Flx_types.btypecode_t list ->
  Flx_srcref.t ->
  Flx_ctypes.cexpr_t

(* for use in an expression *)
val get_var_ref:
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  Flx_types.bid_t ->
  Flx_types.btypecode_t list ->
  string

(* for definition/initialisation *)
val get_ref_ref:
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  Flx_types.bid_t ->
  Flx_types.btypecode_t list ->
  string
