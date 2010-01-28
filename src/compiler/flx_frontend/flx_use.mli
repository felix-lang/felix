(** Elide unused entries
 *
 * Name binding pass 2. *)

val find_roots:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  Flx_btype.biface_t list ->
  unit

(*
val uses_type:
  sym_state_t ->
  BidSet.t ref ->
  Flx_bsym_table.t ->
  bool -> (* count inits *)
  Flx_btype.t ->
  unit

val uses_tbexpr:
  sym_state_t ->
  BidSet.t ref ->
  Flx_bsym_table.t ->
  bool -> (* count inits *)
  Flx_bexpr.t ->
  unit

val uses:
  sym_state_t ->
  BidSet.t ref ->
  Flx_bsym_table.t ->
  bool -> (* true to count initialisations as uses *)
  int ->
  unit
*)

(* counts initialisation as use *)
val full_use_closure_for_symbols:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t list -> (* The list of symbols to count usage for. *)
  Flx_types.BidSet.t

(* counts initialisation as use *)
val full_use_closure:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_types.BidSet.t

(* conditionally count initialisation as use *)
val cal_use_closure:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  bool ->
  Flx_types.BidSet.t

val copy_used:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_bsym_table.t
