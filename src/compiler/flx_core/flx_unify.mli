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
  bool -> (* true if lvalue degrade is allowed *)
  int ref -> (* alpha conversion counter *)
  symbol_table_t -> (* just for diagnostics *)
  (btypecode_t * btypecode_t) list ->
  IntSet.t -> (* dependent variable set *)
  (int * btypecode_t) list


(** obtain the mgu of a set of type equations *)
val maybe_unification:
  int ref -> (* alpha conversion counter *)
  symbol_table_t -> (* just for diagnostics *)
  (btypecode_t * btypecode_t) list ->
  (int * btypecode_t) list option

(** obtain the mgu of a set of type equations,
for matching parameters against arguments: allows
special subtyping for lvalues
*)
val maybe_matches:
  int ref -> (* alpha conversion counter *)
  symbol_table_t -> (* just for diagnostics *)
  (btypecode_t * btypecode_t) list ->
  (int * btypecode_t) list option

val maybe_specialisation:
  int ref -> (* alpha conversion counter *)
  symbol_table_t -> (* just for diagnostics *)
  (btypecode_t * btypecode_t) list ->
  (int * btypecode_t) list option

(** test if two types unify *)
val unifies:
  int ref -> (* alpha conversion counter *)
  symbol_table_t -> (* just for diagnostics *)
  btypecode_t ->
  btypecode_t ->
  bool

(** compare type for structural/unificational ordering *)
val compare_sigs:
  int ref -> (* alpha conversion counter *)
  symbol_table_t ->
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
  btypecode_t ->
  btypecode_t ->
  bool

(** compare for iso-equality *)
val type_eq:
  int ref -> (* alpha conversion counter *)
  symbol_table_t ->
  btypecode_t ->
  btypecode_t ->
  bool

(** compare parameter with argument type,
  argument may have extra lvalue in it
*)
val type_match:
  int ref -> (* alpha conversion counter *)
  symbol_table_t ->
  btypecode_t ->
  btypecode_t ->
  bool

(** [lstrip t] returns t all lvalue combinators elided
*)
val lstrip:
  symbol_table_t ->
  btypecode_t ->
  btypecode_t

(** [unfold t] returns t with each fix variable
  denoting t replaced with t
*)
val unfold:
  symbol_table_t ->
  btypecode_t ->
  btypecode_t

(** undo an unfold *)
val fold:
  int ref -> (* counter for alpha conversion *)
  symbol_table_t ->
  btypecode_t ->
  btypecode_t

(** minimise the representation of a type
with respect to recursion
*)

val minimise:
  int ref -> (* counter for alpha conversion *)
  symbol_table_t ->
  btypecode_t ->
  btypecode_t

(** replace variables in the term using
the mapping in a list *)
val list_subst:
  int ref -> (* counter for alpha conversion *)
  (int * btypecode_t) list ->
  btypecode_t ->
  btypecode_t

(** make a varmap using vs and ts list *)
val mk_varmap:
  (string * int) list -> (* vs list *)
  btypecode_t list ->  (* ts list *)
  (int,btypecode_t) Hashtbl.t

(** replace variables using vs and ts list to
determine mapping *)
val tsubst :
  (string * int) list -> (* vs list *)
  btypecode_t list ->  (* ts list *)
  btypecode_t ->
  btypecode_t

(** replace variables in the term using
the mapping in the hashtable
*)
val varmap_subst:
  (int,btypecode_t) Hashtbl.t ->
  btypecode_t ->
  btypecode_t

(** check for variables *)
val var_occurs:
  btypecode_t ->
  bool

(** check for a particular variable *)
val var_i_occurs:
  int ->
  btypecode_t ->
  bool

(** check for variables *)
val var_list_occurs:
  int list ->
  btypecode_t ->
  bool

(** normalise returns count of the type variables
  occuring in a type, and the type rewritten so the type variables
  are systematically numbered from 0 - n-1
*)
val normalise_type:
  btypecode_t -> int list * btypecode_t

(** check for bad recursions *)
val check_recursion:
  btypecode_t -> unit

(** construct the dual of a type *)
val dual:
  btypecode_t -> btypecode_t

val expr_maybe_matches:
  int ref -> (* counter for alpha conversion *)
  symbol_table_t -> (* just for diagnostics *)
  int list -> (* type variables *)
  int list -> (* variables *)
  tbexpr_t -> (* match term *)
  tbexpr_t   (* candidate *)
  ->
  (
    (int * btypecode_t) list * (* type mgu *)
    (int * tbexpr_t) list      (* expr mgu *)
  )
  option

val expr_term_subst:
  tbexpr_t -> (* candidate *)
  int ->      (* variable index *)
  tbexpr_t -> (* variable value *)
  tbexpr_t    (* candidate with index -> value *)

val alpha:
  int ref ->
  btypecode_t ->
  btypecode_t
