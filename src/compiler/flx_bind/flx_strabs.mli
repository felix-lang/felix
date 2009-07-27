(** Downgrade abstract types
 *
 * Convert newtype abstractions to their representations. *)

type strabs_state_t

(** Construct the state needed for downgrading abstract types. *)
val make_strabs_state:
  Flx_types.fully_bound_symbol_table_t -> (** The input symbol table *)
  Flx_types.fully_bound_symbol_table_t -> (** The output symbol table *)
  strabs_state_t

val strabs_symbol:
  strabs_state_t -> (** The state needed for downgrading types. *)
  int ->
  Flx_types.symbol_data3_t ->
  Flx_types.symbol_data3_t option

val strabs:
  strabs_state_t -> (** The state needed for downgrading types. *)
  unit
