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
  bool ->                   (** Whether or not to clean up unused symbols. *)
  Flx_types.bsym_table_t * Flx_child.t

(** Prep the bound symbol table for the backend by lowering and simplifying
 * symbols. *)
val lower_symbols :
  frontend_state_t ->
  Flx_types.bsym_table_t -> (* The bound symbol table. *)
  Flx_child.t ->            (* A lookup table for children. *)
  Flx_types.bid_t ->        (* The root symbol. *)
  Flx_types.bexe_t list ->  (* The list of exes to lower. *)
  Flx_types.bid_t list ->   (* The list of symbols to lower. *)
  Flx_types.bsym_table_t *
  Flx_child.t *
  Flx_types.bexe_t list *
  Flx_types.bid_t list

(** Prep the bound symbol table for the backend by lowering and simplifying
 * symbols. *)
val lower_bsym_table :
  frontend_state_t ->
  Flx_types.bsym_table_t -> (* The bound symbol table. *)
  Flx_types.bid_t ->        (* The root symbol. *)
  Flx_types.bsym_table_t * Flx_child.t
