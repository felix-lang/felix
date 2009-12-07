(** Elide unused entries
 *
 * Name binding pass 2. *)

val add_prop:
  Flx_bsym_table.t ->
  Flx_ast.property_t ->
  Flx_types.bid_t ->
  unit

val rem_prop:
  Flx_bsym_table.t ->
  Flx_ast.property_t ->
  Flx_types.bid_t ->
  unit
