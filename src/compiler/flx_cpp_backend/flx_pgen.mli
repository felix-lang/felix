(** C++ code generator *)

val gen_prim_call :
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  (Flx_types.btypecode_t -> Flx_types.btypecode_t) ->
  (Flx_srcref.t -> Flx_types.tbexpr_t -> Flx_ctypes.cexpr_t) ->
  string ->
  Flx_types.btypecode_t list ->
  Flx_types.tbexpr_t ->
  string ->
  Flx_srcref.t ->
  Flx_srcref.t ->
  string ->
  Flx_ctypes.cexpr_t

val shape_of:
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  (Flx_types.btypecode_t -> string) ->
  Flx_types.btypecode_t ->
  string
