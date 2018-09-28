(** Inline exes *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call
open Flx_bid

type submode_t = [`Eager | `Lazy]

val gen_body :
  sym_state_t ->
  usage_table_t -> Flx_bsym_table.t ->
  string ->                         (* name *)
  Flx_bparams.t ->                  (* parameters *)
  (bid_t, bid_t) Hashtbl.t ->       (* revariable *)
  Flx_bexe.t list ->                (* the exes *)
  Flx_bexpr.t ->                    (* argument *)
  Flx_srcref.t ->                   (* srcref *)
  bid_t ->                          (* caller *)
  bid_t ->                          (* callee *)
  submode_t ->                      (* default arg passing mode *)
  property_t list ->                (* properties *)
  Flx_bexe.t list

val recal_exes_usage:
  usage_table_t ->
  Flx_srcref.t ->
  bid_t ->
  Flx_bparams.t ->
  Flx_bexe.t list ->
  unit
