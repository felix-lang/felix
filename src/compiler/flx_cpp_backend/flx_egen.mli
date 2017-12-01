open Flx_bid

val gen_expr:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_set.StringSet.t ref ->
  (string, Flx_btype.t) Hashtbl.t ->
  Flx_label.label_info_t ->
  bid_t ->
  Flx_kind.bvs_t ->
  Flx_btype.t list ->
  Flx_srcref.t ->
  Flx_bexpr.t ->
  string

val gen_expr':
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_set.StringSet.t ref ->
  (string, Flx_btype.t) Hashtbl.t ->
  Flx_label.label_info_t ->
  bid_t ->
  Flx_kind.bvs_t ->
  Flx_btype.t list ->
  Flx_srcref.t ->
  Flx_bexpr.t ->
  Flx_ctypes.cexpr_t

(* for use in an expression *)
val get_var_ref:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  bid_t ->
  bid_t ->
  Flx_btype.t list ->
  string

(* for definition/initialisation *)
val get_ref_ref:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  bid_t ->
  bid_t ->
  Flx_btype.t list ->
  string
