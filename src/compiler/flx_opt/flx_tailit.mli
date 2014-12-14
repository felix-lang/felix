open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call

val tailit:
  sym_state_t ->
  Flx_bsym_table.t ->
  usage_table_t ->
  string ->
  bid_t ->
  Flx_srcref.t ->
  Flx_bparameter.t list ->
  Flx_bexe.t list ->
  Flx_bexe.t list

val exes_get_xclosures:
  sym_state_t ->
  Flx_bexe.t list ->
  BidSet.t
