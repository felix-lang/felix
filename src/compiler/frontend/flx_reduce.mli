(** Reductions *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

val remove_useless_reductions:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  reduction_t list ->
  reduction_t list

val reduce_exes:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  reduction_t list ->
  bexe_t list ->
  bexe_t list
