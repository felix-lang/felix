(** Generic support *)

val find_split_vs:
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  Flx_types.plain_ivs_list_t *
  Flx_types.plain_ivs_list_t *
  Flx_ast.vs_aux_t

val find_vs:
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  Flx_types.ivs_list_t

val adjust_ts:
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_srcref.t ->
  Flx_types.bid_t ->
  Flx_btype.t list ->
  Flx_btype.t list

val make_params:
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_srcref.t ->
  Flx_types.bid_t ->
  Flx_btype.t list ->
  (string * Flx_btype.t) list

val make_varmap:
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_srcref.t ->
  Flx_types.bid_t ->
  Flx_btype.t list ->
  (Flx_types.bid_t, Flx_btype.t) Hashtbl.t
