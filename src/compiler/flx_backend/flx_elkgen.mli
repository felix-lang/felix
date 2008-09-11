open Flx_ast
open Flx_types
open Flx_mtypes2

val gen_elk_parser:
  string ->
  string ->
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int ->
  range_srcref ->
  btypecode_t ->
  int ->
  int list ->
  unit

val gen_elk_lexer:
  string ->
  string ->
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int ->
  range_srcref ->
  tbexpr_t ->
  int ->
  unit
