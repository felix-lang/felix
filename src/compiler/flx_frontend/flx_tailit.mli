open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call
open Flx_srcref
open Flx_child

val tailit:
  sym_state_t ->
  usage_table_t * child_map_t * fully_bound_symbol_table_t ->
  string ->
  bid_t ->
  Flx_srcref.t ->
  bparameter_t list ->
  bvs_t ->
  bexe_t list ->
  bexe_t list

val exes_get_xclosures:
  sym_state_t ->
  bexe_t list ->
  IntSet.t
