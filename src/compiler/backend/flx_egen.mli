open Flx_ast
open Flx_types
open Flx_mtypes2
open Flx_ctypes

val gen_expr:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int ->
  tbexpr_t ->
  bvs_t ->
  btypecode_t list ->
  range_srcref -> string

val gen_expr':
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int ->
  tbexpr_t ->
  bvs_t ->
  btypecode_t list ->
  range_srcref -> cexpr_t

(* for use in an expression *)
val get_var_ref:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int ->
  int ->
  btypecode_t list ->
  string

(* for definition/initialisation *)
val get_ref_ref:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int ->
  int ->
  btypecode_t list ->
  string
