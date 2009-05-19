(** Optimisation stuff
 *
 * Name binding pass 2. *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

type usage_table_t =  (bid_t, (bid_t * Flx_srcref.t) list) Hashtbl.t
type usage_t =  usage_table_t * usage_table_t

val call_data:
  sym_state_t -> fully_bound_symbol_table_t -> usage_t

val print_call_report:
  sym_state_t -> fully_bound_symbol_table_t -> out_channel -> unit

val is_recursive_call: usage_table_t -> bid_t -> bid_t -> bool
val is_recursive: usage_table_t -> bid_t -> bool

val cal_exe_usage:
  sym_state_t -> usage_table_t -> int -> bexe_t -> unit

val cal_expr_usage:
  sym_state_t -> usage_table_t -> int -> Flx_srcref.t -> tbexpr_t -> unit

val cal_param_usage:
  sym_state_t ->
  usage_table_t ->
  Flx_srcref.t ->
  int ->
  bparameter_t ->
  unit

val use_closure:
  usage_table_t -> int -> IntSet.t

val child_use_closure:
  IntSet.t -> usage_table_t -> int -> IntSet.t

val expr_uses:
 sym_state_t ->
 IntSet.t ->
 usage_table_t ->
 IntSet.t ->
 tbexpr_t ->
 IntSet.t

val expr_uses_unrestricted:
 sym_state_t ->
 IntSet.t ->
 usage_table_t ->
 tbexpr_t ->
 IntSet.t
