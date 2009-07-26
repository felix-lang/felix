type bind_state_t

val make_bind_state: Flx_mtypes2.sym_state_t -> bind_state_t

(* Bind all the symbols. *)
val bind_asms:
  bind_state_t ->
  Flx_types.asm_t list ->
  Flx_types.fully_bound_symbol_table_t
