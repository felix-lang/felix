(** Monomorphism
 *
 * This requires the instantiator to have been run. *)

open Flx_ast
open Flx_types
open Flx_mtypes1
open Flx_mtypes2
open Flx_call

val monomorphise:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  unit
