(** Elide unused entries
 *
 * Name binding pass 2. *)

val add_prop:
  Flx_types.bsym_table_t ->
  Flx_ast.property_t ->
  Flx_types.bid_t ->
  unit

val rem_prop:
  Flx_types.bsym_table_t ->
  Flx_ast.property_t ->
  Flx_types.bid_t ->
  unit

val get_vs:
  Flx_types.bsym_table_t ->
  Flx_types.bid_t ->
  Flx_types.bvs_t
