(** Make stack calls
 *
 * Name binding pass 2 *)

val make_stack_calls:
  Flx_mtypes2.sym_state_t ->
  Flx_types.bsym_table_t ->
  Flx_child.t ->
  Flx_label.label_map_t ->
  Flx_label.label_usage_t ->
  unit

val can_stack_func:
  Flx_mtypes2.sym_state_t ->
  Flx_types.bsym_table_t ->
  Flx_child.t ->
  (Flx_types.btypecode_t, [`Recurse | `Safe | `Unsafe]) Hashtbl.t ->
  (Flx_types.btypecode_t, [`Recurse | `Safe | `Unsafe]) Hashtbl.t ->
  Flx_types.bid_t ->
  bool
