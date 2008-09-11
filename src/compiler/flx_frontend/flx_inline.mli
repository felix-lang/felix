(** Inlining
 *
 * To make this work, we need a theorem.  First, the call rule is:
 *
 * [A procedure may only call a child of an ancestor.]
 *
 * Note an ancestor is itself or a parent of any ancestor: that is, a procedure
 * is an ancestor of itself. A parentless toplevel procedure is considered a
 * child of a dummy root to make this simple formulation work.
 *
 * It is clear we can inline any sibling by copying its body, and duplicating
 * any children -- variables and nested procedures included. This is because
 * any references to its parent will go through from the caller, since they
 * have the same parent. 
 *
 * Clearly this result extends to any child of any parent. *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call

val heavy_inlining:
  sym_state_t ->
  (bid_t, bid_t list) Hashtbl.t *
  fully_bound_symbol_table_t ->
  unit
