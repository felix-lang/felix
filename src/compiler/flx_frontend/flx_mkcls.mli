(** Construct closures for functions.
 *
 * Name binding pass 2.
 * 
 * These routines lift lambda abstractions (implied or explicit)
 * out of the code. For example function compositions, using
 * a union constructor as a function, etc.
 * *)

type closure_state_t

(** Create state needed for constructing closures. *)
val make_closure_state :
  Flx_mtypes2.sym_state_t ->
  closure_state_t

(** Extract all closures. *)
val make_closures :
  closure_state_t ->
  Flx_bsym_table.t ->
  unit

(** Extract all closures. *)
val premake_closures :
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  unit 
