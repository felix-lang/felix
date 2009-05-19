(** Name binding
 *
 * Name binding pass 2 *)

val bbind:
  Flx_mtypes2.sym_state_t ->
  Flx_types.fully_bound_symbol_table_t

val bind_ifaces:
  Flx_mtypes2.sym_state_t ->
  (Flx_srcref.t * Flx_types.iface_t * int option) list ->
  Flx_types.biface_t list
