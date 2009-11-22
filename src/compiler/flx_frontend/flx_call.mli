(** Optimisation stuff
 *
 * Name binding pass 2. *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

type usage_table_t =  (bid_t, (bid_t * Flx_srcref.t) list) Hashtbl.t
type usage_t =  usage_table_t * usage_table_t

(** [call_data_for_symbol bsym_table uses index symbol] adds the indexes of the calls
 * [index] makes to the [uses] table. *)
val call_data_for_symbol:
  Flx_bsym_table.t ->
  usage_table_t ->
  Flx_types.bid_t ->
  Flx_bsym.t ->
  unit

(** [call_data bsym_table] returns tables of all the calls, and the calls they
 * make. *)
val call_data:
  Flx_bsym_table.t -> usage_t

val print_call_report:
  sym_state_t -> Flx_bsym_table.t -> out_channel -> unit

val is_recursive_call: usage_table_t -> bid_t -> bid_t -> bool
val is_recursive: usage_table_t -> bid_t -> bool

val cal_exe_usage:
  usage_table_t -> bid_t -> bexe_t -> unit

val cal_expr_usage:
  usage_table_t -> bid_t -> Flx_srcref.t -> tbexpr_t -> unit

val cal_param_usage:
  usage_table_t ->
  Flx_srcref.t ->
  bid_t ->
  bparameter_t ->
  unit

val use_closure:
  usage_table_t -> bid_t -> BidSet.t

val child_use_closure:
  BidSet.t -> usage_table_t -> bid_t -> BidSet.t

val expr_uses:
 sym_state_t ->
 BidSet.t ->
 usage_table_t ->
 BidSet.t ->
 tbexpr_t ->
 BidSet.t

val expr_uses_unrestricted:
 sym_state_t ->
 BidSet.t ->
 usage_table_t ->
 tbexpr_t ->
 BidSet.t
