open Flx_ast
open Flx_types
open Flx_mtypes2

(*
val type_of_tpattern:
  sym_state_t -> tpattern_t ->
  typecode_t *
  (int * string) list *     (* variables for '?' terms *)
  int list *                (* variables for 'any' terms *)
  (int * string) list *     (* variables for 'as' terms *)
  (int * typecode_t) list   (* variables for as terms *)
*)

val type_of_tpattern:
  sym_state_t -> typecode_t ->
  typecode_t *
  (int * string) list *     (* variables for '?' terms *)
  int list *                (* variables for 'any' terms *)
  (int * string) list *     (* variables for 'as' terms *)
  (int * typecode_t) list   (* variables for as terms *)
