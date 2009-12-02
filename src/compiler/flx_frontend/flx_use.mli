(** Elide unused entries
 *
 * Name binding pass 2. *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

val find_roots:
  sym_state_t ->
  Flx_bsym_table.t ->
  bid_t ->
  biface_t list -> unit

(*
val uses_type:
  sym_state_t ->
  BidSet.t ref ->
  Flx_bsym_table.t ->
  bool -> (* count inits *)
  btypecode_t ->
  unit

val uses_tbexpr:
  sym_state_t ->
  BidSet.t ref ->
  Flx_bsym_table.t ->
  bool -> (* count inits *)
  tbexpr_t ->
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
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_types.bid_t list -> (* The list of symbols to count usage for. *)
  BidSet.t

(* counts initialisation as use *)
val full_use_closure:
  sym_state_t ->
  Flx_bsym_table.t ->
  BidSet.t

(* conditionally count initialisation as use *)
val cal_use_closure:
  sym_state_t ->
  Flx_bsym_table.t ->
  bool ->
  BidSet.t

val copy_used:
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_bsym_table.t
