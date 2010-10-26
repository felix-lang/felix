(* Desugaring
 *
 * Two routines: one to build interfaces from modules, and one to lift lambdas
 * and also blocks.
 *)

open Flx_ast
open Flx_types
open Flx_mtypes2

val collate_namespaces:
  sym_state_t -> statement_t list -> statement_t list

val include_file:
  sym_state_t ->
  string -> (* directory containing including file,
             * replaces leading . in request name 
             *)
  string -> (* request name *)
  string * (statement_t list) (* return path where found and parsed asms *)
