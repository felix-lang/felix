(** Meta typing. *)
val metatype:
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_srcref.t ->
  Flx_types.btypecode_t ->
  Flx_types.btypecode_t
