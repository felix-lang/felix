(** Name binding
 *
 * Name binding pass 2 *)

type bbind_state_t

val make_bbind_state:
  Flx_mtypes2.sym_state_t ->
  Flx_types.fully_bound_symbol_table_t ->
  bbind_state_t

(* Bind a single symbol. *)
val bbind_symbol:
  bbind_state_t ->
  int ->
  Flx_types.symbol_data_t ->
  Flx_types.symbol_data3_t option

(* Bind all the symbols in the symtab. *)
val bbind: bbind_state_t -> unit

(* Bind a single interface *)
val bind_interface:
  bbind_state_t ->
  Flx_types.bound_iface_t ->
  Flx_types.biface_t
