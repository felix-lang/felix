(** Expression unraveller *)

open Flx_types
open Flx_mtypes2

val unravel:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  tbexpr_t ->
  (tbexpr_t * string) list *
  tbexpr_t
