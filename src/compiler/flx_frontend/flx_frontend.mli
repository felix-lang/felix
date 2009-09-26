type frontend_state_t

(** Create the state needed for the frontend. *)
val make_frontend_state :
  Flx_mtypes2.sym_state_t ->
  frontend_state_t

(** Perform high level platform independent optimizations. *)
val optimize :
  frontend_state_t ->
  Flx_types.bsym_table_t ->
  Flx_types.bid_t ->
  Flx_types.bsym_table_t

(** Prep the bound symbol table for the backend by lowering and simplifying
 * symbols. *)
val lower_symbol :
  frontend_state_t ->
  Flx_types.bsym_table_t ->
  Flx_types.bid_t ->
  Flx_types.bsym_t ->
  Flx_types.bsym_table_t

(** Prep the bound symbol table for the backend by lowering and simplifying
 * symbols. *)
val lower_bsym_table :
  frontend_state_t ->
  Flx_types.bsym_table_t ->
  Flx_types.bid_t ->
  Flx_types.bsym_table_t * Flx_child.t
