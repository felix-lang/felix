(** Make stack calls
 *
 * Name binding pass 2 *)

val make_stack_calls:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_label.label_info_t ->
  unit

val can_stack_func:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  (Flx_btype.t, [`Recurse | `Safe | `Unsafe]) Hashtbl.t ->
  (Flx_btype.t, [`Recurse | `Safe | `Unsafe]) Hashtbl.t ->
  Flx_types.bid_t ->
  bool
