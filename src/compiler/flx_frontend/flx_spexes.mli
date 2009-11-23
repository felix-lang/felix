(** Inline exes *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call

type submode_t = [`Eager | `Lazy]

val gen_body :
  sym_state_t ->
  Flx_sym_table.t ->
  usage_table_t * Flx_child.t * Flx_bsym_table.t ->
  string ->                         (* name *)
  (bid_t, btypecode_t) Hashtbl.t -> (* varmap *)
  bparameter_t list ->              (* parameters *)
  (string, string) Hashtbl.t ->     (* relabel *)
  (bid_t, bid_t) Hashtbl.t ->       (* revariable *)
  bexe_t list ->                    (* the exes *)
  tbexpr_t ->                       (* argument *)
  Flx_srcref.t ->                   (* srcref *)
  bid_t ->                          (* caller *)
  bid_t ->                          (* callee *)
  bvs_t ->                          (* caller vs *)
  int ->                            (* callee vs len *)
  submode_t ->                      (* default arg passing mode *)
  property_t list ->                (* properties *)
  bexe_t list

val recal_exes_usage:
  usage_table_t ->
  Flx_srcref.t ->
  bid_t ->
  bparameter_t list ->
  bexe_t list ->
  unit
