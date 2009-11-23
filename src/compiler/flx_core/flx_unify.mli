(** Unification
 *
 * This module provides type unification and utilities. *)

open Flx_types
open Flx_set
open Flx_mtypes2

(** obtain the mgu of a set of type equations with specified
  dependent variable set, throws Not_found if cannot unify
*)
val unification:
  bid_t ref -> (* alpha conversion counter *)
  Flx_sym_table.t -> (* just for diagnostics *)
  (btypecode_t * btypecode_t) list ->
  BidSet.t -> (* dependent variable set *)
  (bid_t * btypecode_t) list

(** obtain the mgu of a set of type equations *)
val maybe_unification:
  bid_t ref -> (* alpha conversion counter *)
  Flx_sym_table.t -> (* just for diagnostics *)
  (btypecode_t * btypecode_t) list ->
  (bid_t * btypecode_t) list option

(** obtain the mgu of a set of type equations,
for matching parameters against arguments: allows
special subtyping for lvalues
*)
val maybe_matches:
  bid_t ref -> (* alpha conversion counter *)
  Flx_sym_table.t -> (* just for diagnostics *)
  (btypecode_t * btypecode_t) list ->
  (bid_t * btypecode_t) list option

val maybe_specialisation:
  bid_t ref -> (* alpha conversion counter *)
  Flx_sym_table.t -> (* just for diagnostics *)
  (btypecode_t * btypecode_t) list ->
  (bid_t * btypecode_t) list option

(** test if two types unify *)
val unifies:
  bid_t ref -> (* alpha conversion counter *)
  Flx_sym_table.t -> (* just for diagnostics *)
  btypecode_t ->
  btypecode_t ->
  bool

(** compare type for structural/unificational ordering *)
val compare_sigs:
  bid_t ref -> (* alpha conversion counter *)
  Flx_sym_table.t ->
  btypecode_t ->
  btypecode_t ->
  partial_order_result_t

(** check if the two types unify: update the
variable definitions in sym_state ??? Only
useful if type variables are global, which is
the function return type unknown variable case..
*)
val do_unify:
  sym_state_t ->
  Flx_sym_table.t ->
  btypecode_t ->
  btypecode_t ->
  bool

(** compare for iso-equality *)
val type_eq:
  bid_t ref -> (* alpha conversion counter *)
  Flx_sym_table.t ->
  btypecode_t ->
  btypecode_t ->
  bool

(** compare parameter with argument type,
  argument may have extra lvalue in it
*)
val type_match:
  bid_t ref -> (* alpha conversion counter *)
  Flx_sym_table.t ->
  btypecode_t ->
  btypecode_t ->
  bool

(** [unfold t] returns t with each fix variable
  denoting t replaced with t
*)
val unfold:
  Flx_sym_table.t ->
  btypecode_t ->
  btypecode_t

(** undo an unfold *)
val fold:
  bid_t ref -> (* counter for alpha conversion *)
  Flx_sym_table.t ->
  btypecode_t ->
  btypecode_t

(** minimise the representation of a type
with respect to recursion
*)

val minimise:
  bid_t ref -> (* counter for alpha conversion *)
  Flx_sym_table.t ->
  btypecode_t ->
  btypecode_t

(** replace variables in the term using
the mapping in a list *)
val list_subst:
  bid_t ref -> (* counter for alpha conversion *)
  (bid_t * btypecode_t) list ->
  btypecode_t ->
  btypecode_t

(** make a varmap using vs and ts list *)
val mk_varmap:
  (string * bid_t) list -> (* vs list *)
  btypecode_t list ->  (* ts list *)
  (bid_t, btypecode_t) Hashtbl.t

(** replace variables using vs and ts list to
determine mapping *)
val tsubst :
  (string * bid_t) list -> (* vs list *)
  btypecode_t list ->  (* ts list *)
  btypecode_t ->
  btypecode_t

(** replace variables in the term using
the mapping in the hashtable
*)
val varmap_subst:
  (bid_t, btypecode_t) Hashtbl.t ->
  btypecode_t ->
  btypecode_t

(** check for variables *)
val var_occurs:
  btypecode_t ->
  bool

(** check for a particular variable *)
val var_i_occurs:
  bid_t ->
  btypecode_t ->
  bool

(** check for variables *)
val var_list_occurs:
  bid_t list ->
  btypecode_t ->
  bool

(** check for bad recursions *)
val check_recursion:
  btypecode_t -> unit

(** construct the dual of a type *)
val dual:
  btypecode_t -> btypecode_t

val expr_maybe_matches:
  bid_t ref -> (* counter for alpha conversion *)
  Flx_sym_table.t -> (* just for diagnostics *)
  bid_t list -> (* type variables *)
  bid_t list -> (* variables *)
  tbexpr_t -> (* match term *)
  tbexpr_t   (* candidate *)
  ->
  (
    (bid_t * btypecode_t) list * (* type mgu *)
    (bid_t * tbexpr_t) list      (* expr mgu *)
  )
  option

val expr_term_subst:
  tbexpr_t -> (* candidate *)
  bid_t ->    (* variable index *)
  tbexpr_t -> (* variable value *)
  tbexpr_t    (* candidate with index -> value *)

val alpha:
  bid_t ref ->
  btypecode_t ->
  btypecode_t
