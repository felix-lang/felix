open Flx_ast
open Flx_types
open Flx_mtypes2
val desugar_program:
  sym_state_t ->
  string ->
  statement_t list ->
  asm_t list
