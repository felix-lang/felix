(** Elide unused entries
 *
 * Name binding pass 2. *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

val find_roots:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  bid_t ->
  biface_t list -> unit

(*
val uses_type:
  sym_state_t ->
  IntSet.t ref ->
  fully_bound_symbol_table_t ->
  bool -> (* count inits *)
  btypecode_t ->
  unit

val uses_tbexpr:
  sym_state_t ->
  IntSet.t ref ->
  fully_bound_symbol_table_t ->
  bool -> (* count inits *)
  tbexpr_t ->
  unit

val uses:
  sym_state_t ->
  IntSet.t ref ->
  fully_bound_symbol_table_t ->
  bool -> (* true to count initialisations as uses *)
  int ->
  unit
*)

(* counts initialisation as use *)
val full_use_closure:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  IntSet.t

(* conditionally count initialisation as use *)
val cal_use_closure:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  bool ->
  IntSet.t

val copy_used:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  fully_bound_symbol_table_t
