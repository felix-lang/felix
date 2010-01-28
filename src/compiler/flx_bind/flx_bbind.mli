(** Name binding
 *
 * Name binding pass 2 *)

type bbind_state_t

(** The state needed for binding. *)
val make_bbind_state:
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_lookup.lookup_state_t ->
  bbind_state_t

(* Bind a single symbol. *)
val bbind_symbol:
  bbind_state_t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  Flx_sym.t ->
  Flx_bsym.t option

(* Bind all the symbols in the symtab. *)
val bbind:
  bbind_state_t ->
  Flx_bsym_table.t ->
  unit

(* Bind a single interface *)
val bind_interface:
  bbind_state_t ->
  Flx_bsym_table.t ->
  Flx_types.bound_iface_t ->
  Flx_btype.biface_t
