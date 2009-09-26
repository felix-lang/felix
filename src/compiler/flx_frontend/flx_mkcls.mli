(** Construct closures for functions.
 *
 * Name binding pass 2. *)

type closure_state_t

(** Create state needed for constructing closures. *)
val make_closure_state :
  Flx_mtypes2.sym_state_t ->
  Flx_types.bsym_table_t ->
  closure_state_t

(** Extract all closures. *)
val make_closure :
  closure_state_t ->
  Flx_types.bid_t ->
  Flx_types.bsym_t ->
  unit

(** Extract all closures. *)
val make_closures :
  closure_state_t ->
  unit
