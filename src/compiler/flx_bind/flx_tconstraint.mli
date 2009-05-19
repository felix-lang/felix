open Flx_ast
open Flx_types
open Flx_mtypes2

val build_type_constraints:
  sym_state_t ->
  (typecode_t -> btypecode_t) -> (* bind type *)
  Flx_srcref.t ->
  (string * int * typecode_t) list -> (* local vs list *)
  btypecode_t
