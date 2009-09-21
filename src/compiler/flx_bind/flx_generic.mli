(** Generic support *)

val find_split_vs:
  Flx_types.symbol_table_t ->
  int ->
  Flx_types.plain_ivs_list_t *
  Flx_types.plain_ivs_list_t *
  Flx_ast.vs_aux_t

val find_vs:
  Flx_types.symbol_table_t ->
  int ->
  Flx_types.ivs_list_t

val adjust_ts:
  Flx_types.symbol_table_t ->
  Flx_srcref.t ->
  int ->
  Flx_types.btypecode_t list ->
  Flx_types.btypecode_t list

val make_params:
  Flx_types.symbol_table_t ->
  Flx_srcref.t ->
  int ->
  Flx_types.btypecode_t list ->
  (string * Flx_types.btypecode_t) list

val make_varmap:
  Flx_types.symbol_table_t ->
  Flx_srcref.t ->
  int ->
  Flx_types.btypecode_t list ->
  (int, Flx_types.btypecode_t) Hashtbl.t
