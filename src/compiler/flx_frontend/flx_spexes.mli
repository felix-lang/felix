(** Inline exes *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call
open Flx_child

type submode_t = [`Eager | `Lazy]

val gen_body :
  sym_state_t ->
  usage_table_t * child_map_t * fully_bound_symbol_table_t ->
  string ->                        (* name *)
  (int, btypecode_t) Hashtbl.t ->  (* varmap *)
  bparameter_t list ->             (* parameters *)
  (string, string) Hashtbl.t ->    (* relabel *)
  (bid_t, bid_t) Hashtbl.t ->      (* revariable *)
  bexe_t list ->                   (* the exes *)
  tbexpr_t ->                      (* argument *)
  Flx_srcref.t ->                  (* srcref *)
  int ->                           (* caller *)
  bid_t ->                         (* callee *)
  bvs_t ->                         (* caller vs *)
  int ->                           (* callee vs len *)
  submode_t ->                     (* default arg passing mode *)
  property_t list ->               (* properties *)
  bexe_t list

val recal_exes_usage:
  sym_state_t ->
  usage_table_t ->
  Flx_srcref.t ->
  int ->
  bparameter_t list ->
  bexe_t list ->
  unit
