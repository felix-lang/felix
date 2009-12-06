(** Downgrade abstract types
 *
 * Convert newtype abstractions to their representations. *)

type strabs_state_t

(** Construct the state needed for downgrading abstract types. *)
val make_strabs_state: unit -> strabs_state_t

(** Downgrades newtypes to their actual type in the symbol table. *)
val strabs_symbol:
  strabs_state_t ->   (** The state needed for downgrading types. *)
  Flx_bsym_table.t -> (** The symbol table. *)
  Flx_types.bid_t ->  (** The symbol index. *)
  Flx_bsym.t ->       (** The symbol to lower. *)
  unit

(** Downgrades newtypes to their actual type in the bound symbol table. *)
val strabs:
  strabs_state_t ->   (** The state needed for downgrading types. *)
  Flx_bsym_table.t -> (** The symbol table. *)
  unit
