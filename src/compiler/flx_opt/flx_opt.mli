(** Perform high level optimizations across the entire program. *)
val optimize_bsym_table :
  Flx_mtypes2.sym_state_t ->  (** The felix state. *)
  Flx_types.bsym_table_t ->   (** The bound symbol table. *)
  Flx_types.bid_t ->          (** The root procedure. *)
  Flx_types.bsym_table_t * Flx_child.t

(** Perform high level optimizations across the given set of bound exes and
 * symbols.*)
val optimize :
  Flx_mtypes2.sym_state_t ->  (** The felix state. *)
  Flx_types.bsym_table_t ->   (** The bound symbol table. *)
  Flx_child.t ->              (** The map of children. *)
  Flx_types.bid_t ->          (** The root procedure. *)
  Flx_types.bid_t list ->     (** The symbols to optimize. *)
  Flx_types.bexe_t list ->    (** The symbols to optimize. *)
  Flx_types.bsym_table_t * Flx_child.t *
  Flx_types.bid_t list * Flx_types.bexe_t list
