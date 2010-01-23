type lower_state_t

(** Create the state needed for the frontend. *)
val make_lower_state :
  Flx_mtypes2.sym_state_t ->
  lower_state_t


(** Prep the bound symbol table for the backend by lowering and simplifying
 * symbols. *)
val lower_bsym_table :
  lower_state_t ->    (** The felix state. *)
  Flx_bsym_table.t -> (** The bound symbol table. *)
  Flx_types.bid_t ->  (** The root symbol. *)
  Flx_bsym_table.t * Flx_child.t


(** Prep the bound symbol table for the backend by lowering and simplifying
 * symbols. *)
val lower :
  lower_state_t ->          (** The felix state. *)
  Flx_bsym_table.t ->       (** The bound symbol table. *)
  Flx_child.t ->            (** A lookup table for children. *)
  Flx_types.bid_t ->        (** The root symbol. *)
  Flx_types.bid_t list ->   (** The list of symbols to lower. *)
  Flx_bexe.t list ->        (** The list of exes to lower. *)
  Flx_bsym_table.t * Flx_child.t *
  Flx_types.bid_t list * Flx_bexe.t list
