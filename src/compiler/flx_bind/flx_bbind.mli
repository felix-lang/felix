(** Name binding
 *
 * Name binding pass 2 *)

type bbind_state_t

(** The state needed for binding. *)
open Flx_types
val make_bbind_state:
  counter:Flx_types.bid_t ref ->
  print_flag:bool ->
  ticache : (bid_t, Flx_btype.t) Hashtbl.t ->
  varmap : Flx_mtypes2.typevarmap_t ->
  typeclass_to_instance: (bid_t, (bvs_t * Flx_btype.t * Flx_btype.t list * bid_t) list) Hashtbl.t ->
  instances_of_typeclass: (bid_t, (bid_t * (bvs_t * Flx_btype.t * Flx_btype.t list)) list) Hashtbl.t ->
  sym_table:Flx_sym_table.t ->
  axioms: Flx_mtypes2.axiom_t list ref ->
  reductions: Flx_mtypes2.reduction_t list ref ->
  lookup_state:Flx_lookup.lookup_state_t ->
  bbind_state_t

(* Bind a single symbol. *)
val bbind_symbol:
  bbind_state_t ->
  Flx_bsym_table.t ->       (** The bound symbol table that we'll bind into. *)
  Flx_types.bid_t ->        (** The symbol's index. *)
  Flx_types.bid_t option -> (** The symbol's parent. *)
  Flx_sym.t ->              (** The symbol. *)
  unit

(* Bind all the symbols in the symtab. *)
val bbind:
  bbind_state_t ->
  Flx_bsym_table.t ->
  unit

(* Bind a single interface *)
val bind_interface:
  bbind_state_t ->
  Flx_bsym_table.t ->
  Flx_types.bound_iface_t ->
  Flx_btype.biface_t
