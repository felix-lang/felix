(** Downgrade abstract types
 *
 * Convert newtype abstractions to their representations. *)

type strabs_state_t

(** Construct the state needed for downgrading abstract types. *)
val make_strabs_state: unit -> strabs_state_t

val strabs_symbol:
  strabs_state_t ->         (** The state needed for downgrading types. *)
  Flx_types.bsym_table_t -> (** The input symbol table. *)
  Flx_types.bsym_table_t -> (** The output symbol table. *)
  Flx_types.bid_t ->        (** The symbol index. *)
  Flx_types.bsym_t ->       (** The symbol to lower. *)
  Flx_types.bsym_t option

val strabs:
  strabs_state_t ->         (** The state needed for downgrading types. *)
  Flx_types.bsym_table_t -> (** The input symbol table. *)
  Flx_types.bsym_table_t    (** The output symbol table. *)
