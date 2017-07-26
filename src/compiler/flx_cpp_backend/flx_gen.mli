(** C++ code generator *)
open Flx_bid

val gen_function_names:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  string

val gen_functions:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_set.StringSet.t ref ->
  (string, Flx_btype.t) Hashtbl.t ->
  string

val gen_execute_methods:
  string ->
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_set.StringSet.t ref ->
  (string, Flx_btype.t) Hashtbl.t ->
  Flx_label.label_info_t ->
  bid_t ref ->
  out_channel ->
  out_channel ->
  unit

