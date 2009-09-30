(** Construct closures for functions.
 *
 * Name binding pass 2. *)

type closure_state_t

(** Create state needed for constructing closures. *)
val make_closure_state :
  Flx_mtypes2.sym_state_t ->
  closure_state_t

(** Extract all closures. *)
val make_closure :
  closure_state_t ->
  Flx_types.bsym_table_t ->
  Flx_types.bid_t list -> (* The list of symbols to possibly make closures
                           * for. *)
  Flx_types.bid_t list

(** Extract all closures. *)
val make_closures :
  closure_state_t ->
  Flx_types.bsym_table_t ->
  unit
