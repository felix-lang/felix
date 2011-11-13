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
  (Flx_btype.t * Flx_btype.t) list ->
  BidSet.t -> (* dependent variable set *)
  (bid_t * Flx_btype.t) list

(** obtain the mgu of a set of type equations *)
val maybe_unification:
  bid_t ref -> (* alpha conversion counter *)
  (Flx_btype.t * Flx_btype.t) list ->
  (bid_t * Flx_btype.t) list option

(** obtain the mgu of a set of type equations,
for matching parameters against arguments: allows
special subtyping for lvalues
*)
val maybe_matches:
  bid_t ref -> (* alpha conversion counter *)
  (Flx_btype.t * Flx_btype.t) list ->
  (bid_t * Flx_btype.t) list option

val maybe_specialisation:
  bid_t ref -> (* alpha conversion counter *)
  (Flx_btype.t * Flx_btype.t) list ->
  (bid_t * Flx_btype.t) list option

(** test if two types unify *)
val unifies:
  bid_t ref -> (* alpha conversion counter *)
  Flx_btype.t ->
  Flx_btype.t ->
  bool

(** compare type for structural/unificational ordering *)
val compare_sigs:
  bid_t ref -> (* alpha conversion counter *)
  Flx_btype.t ->
  Flx_btype.t ->
  partial_order_result_t

(** compare for iso-equality *)
val type_eq:
  bid_t ref -> (* alpha conversion counter *)
  Flx_btype.t ->
  Flx_btype.t ->
  bool

(** compare parameter with argument type,
  argument may have extra lvalue in it
*)
val type_match:
  bid_t ref -> (* alpha conversion counter *)
  Flx_btype.t ->
  Flx_btype.t ->
  bool

(** [unfold t] returns t with each fix variable
  denoting t replaced with t
*)
val unfold:
  Flx_btype.t ->
  Flx_btype.t

(** undo an unfold *)
val fold:
  bid_t ref -> (* counter for alpha conversion *)
  Flx_btype.t ->
  Flx_btype.t

(** minimise the representation of a type
with respect to recursion
*)

val minimise:
  bid_t ref -> (* counter for alpha conversion *)
  Flx_btype.t ->
  Flx_btype.t

(** replace variables in the term using
the mapping in a list *)
val list_subst:
  bid_t ref -> (* counter for alpha conversion *)
  (bid_t * Flx_btype.t) list ->
  Flx_btype.t ->
  Flx_btype.t

(** make a varmap using vs and ts list *)
val mk_varmap:
  (string * bid_t) list -> (* vs list *)
  Flx_btype.t list ->  (* ts list *)
  (bid_t, Flx_btype.t) Hashtbl.t

(** replace variables using vs and ts list to
determine mapping *)
val tsubst :
  (string * bid_t) list -> (* vs list *)
  Flx_btype.t list ->  (* ts list *)
  Flx_btype.t ->
  Flx_btype.t

(** replace variables in the term using
the mapping in the hashtable
*)
val varmap_subst:
  (bid_t, Flx_btype.t) Hashtbl.t ->
  Flx_btype.t ->
  Flx_btype.t
  
(** check for variables *)
val var_occurs:
  Flx_btype.t ->
  bool

(** check for a particular variable *)
val var_i_occurs:
  bid_t ->
  Flx_btype.t ->
  bool

(** check for variables *)
val var_list_occurs:
  bid_t list ->
  Flx_btype.t ->
  bool

(** check for bad recursions *)
val check_recursion:
  Flx_btype.t -> unit

(** construct the dual of a type *)
val dual:
  Flx_btype.t -> Flx_btype.t

val expr_maybe_matches:
  bid_t ref ->    (* counter for alpha conversion *)
  bid_t list ->   (* type variables *)
  bid_t list ->   (* variables *)
  Flx_bexpr.t ->  (* match term *)
  Flx_bexpr.t     (* candidate *)
  ->
  (
    (bid_t * Flx_btype.t) list *  (* type mgu *)
    (bid_t * Flx_bexpr.t) list    (* expr mgu *)
  )
  option

val expr_term_subst:
  Flx_bexpr.t ->  (* candidate *)
  bid_t ->        (* variable index *)
  Flx_bexpr.t ->  (* variable value *)
  Flx_bexpr.t     (* candidate with index -> value *)

val alpha:
  bid_t ref ->
  Flx_btype.t ->
  Flx_btype.t
