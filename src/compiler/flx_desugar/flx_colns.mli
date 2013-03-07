open Flx_ast
open Flx_types
open Flx_mtypes2

val ocs2flx: Ocs_types.sval -> statement_t

val render: string list -> string -> string list
(*
val include_file:
  sym_state_t ->
  string -> (* directory containing including file,
             * replaces leading . in request name 
             *)
  string -> (* request name *)
  string * (statement_t list) (* return path where found and parsed asms *)
*)
