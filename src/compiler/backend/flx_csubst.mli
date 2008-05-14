(** Code fragment inliner *)

open Flx_ast
open Flx_ctypes

val csubst:
  range_srcref ->
  range_srcref ->
  string ->
  cexpr_t ->      (* value argument 'as is' use $t *)
  cexpr_t list -> (* value arguments as strings *)
  string list -> (* types of value arguments as strings *)
  string ->      (* argument type as string *)
  string ->      (* return type as string *)
  string list -> (* generic arguments as strings *)
  string ->      (* precedence *)
  string ->      (* shape of argument *)
  string list -> (* shape of arguments *)
  string list -> (* display EXCLUDING thread frame *)
  string list -> (* shape of generic type arguments as strings *)
  cexpr_t
