(** Name binding
 *
 * Name binding pass 2 *)

type bbind_state_t

val make_bbind_state: Flx_mtypes2.sym_state_t -> bbind_state_t

val bbind:
  bbind_state_t ->
  Flx_types.fully_bound_symbol_table_t

(* Bind a single interface *)
val bind_interface:
  bbind_state_t ->
  Flx_srcref.t * Flx_types.iface_t * int option ->
  Flx_types.biface_t
