(** Thread frame pointer required detector
 *
 * Run after inlining is done. Uses two properties:
 *
 * {[
   * `Uses_global_var
   * `Requires_ptf
   * `Not_requires_ptf
 * ]}
 *
 * The first is a local property, whilst the second is its closure over abstract
 * closure formation -- both explicit closure building and direct calls. The ptf
 * is only required to reference global variables, to pass to constructors of
 * function objects that do so, or to access the garbage collector.
 *
 * Eliding it when not needed is a useful optimisation. The third property is
 * the negation of the second.
 *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call
open Flx_bid

val set_globals:
  Flx_bsym_table.t ->
  unit

val check_global_vars_all_used:
  Flx_bsym_table.t ->
  (bid_t, 'a) Hashtbl.t -> unit
