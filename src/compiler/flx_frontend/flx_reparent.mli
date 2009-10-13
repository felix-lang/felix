(** Reparenting *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call

val vsplice : 'a list -> int -> 'a list -> 'a list

val reparent1 :
  sym_state_t ->
  usage_table_t * Flx_child.t * bsym_table_t ->
  (string, string) Hashtbl.t ->     (* relabel *)
  (bid_t, btypecode_t) Hashtbl.t -> (* varmap *)
  (bid_t, bid_t) Hashtbl.t ->       (* revariable *)
  (string * bid_t) list ->          (* caller vs *)
  int ->                            (* callee vs length *)
  bid_t ->                          (* routine index *)
  bid_t option ->                   (* parent *)
  bid_t ->                          (* new index, perhaps the caller! *)
  bool ->                           (* allow rescan of cloned stuff? *)
  unit

val reparent_children :
  sym_state_t ->
  usage_table_t * Flx_child.t * bsym_table_t ->
  (string * bid_t) list ->          (* caller vs *)
  int ->                            (* callee_vs_len *)
  bid_t ->                          (* routine index *)
  bid_t option ->                   (* parent *)
  (string, string) Hashtbl.t ->     (* relabel *)
  (bid_t, btypecode_t) Hashtbl.t -> (* varmap *)
  bool ->                           (* rescan flag *)
  bid_t list ->                     (* any extra symbols to remap *)
  (bid_t, bid_t) Hashtbl.t          (* returns revariable map *)

val specialise_symbol:
  sym_state_t ->
  usage_table_t * Flx_child.t * bsym_table_t ->
  (string * bid_t) list ->          (* caller vs *)
  int ->                            (* callee_vs_len *)
  bid_t ->                          (* routine index *)
  btypecode_t list ->               (* instantiating types *)
  bid_t option ->                   (* parent *)
  (string, string) Hashtbl.t ->     (* relabel *)
  (bid_t, btypecode_t) Hashtbl.t -> (* varmap *)
  bool ->                           (* rescan flag *)
  bid_t * btypecode_t list          (* result instance *)

val remap_expr :
  sym_state_t ->
  bsym_table_t ->
  (bid_t, btypecode_t) Hashtbl.t ->
  (bid_t, bid_t) Hashtbl.t ->
  btypecode_t list ->
  int ->
  tbexpr_t ->
  tbexpr_t
