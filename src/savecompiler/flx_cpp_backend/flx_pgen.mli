(** C++ code generator *)

val gen_prim_call :
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_set.StringSet.t ref ->
  (string, Flx_btype.t) Hashtbl.t ->
  (Flx_btype.t -> Flx_btype.t) ->
  (Flx_srcref.t -> Flx_bexpr.t -> Flx_ctypes.cexpr_t) ->
  string ->
  Flx_btype.t list ->
  Flx_bexpr.t ->
  Flx_btype.t ->
  Flx_srcref.t ->
  Flx_srcref.t ->
  string ->
  string ->
  Flx_ctypes.cexpr_t

val shape_of:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  (string, Flx_btype.t) Hashtbl.t ->
  (Flx_btype.t -> string) ->
  Flx_btype.t ->
  string

val direct_shape_of:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  (string, Flx_btype.t) Hashtbl.t ->
  (Flx_btype.t -> string) ->
  Flx_btype.t ->
  string
