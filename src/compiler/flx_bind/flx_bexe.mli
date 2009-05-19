(** Bind executable statements *)

val bind_exes:
  Flx_mtypes2.sym_state_t ->
  Flx_types.env_t ->
  Flx_srcref.t ->
  (Flx_srcref.t * Flx_ast.exe_t) list ->
  Flx_types.btypecode_t ->
  string ->
  Flx_ast.bid_t ->
  Flx_types.bvs_t ->
  Flx_types.btypecode_t * Flx_types.bexe_t list
