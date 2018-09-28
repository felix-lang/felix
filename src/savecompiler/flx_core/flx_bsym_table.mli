(** The type of the bound symbol table. *)
open Flx_bid
exception IncompleteBsymTable of int * int * string

type t

(** Construct a bound symbol table. *)
val create_fresh : unit -> t

(* copies subtype map *)
val create_from : t -> t

(* TABLE MUTATORS *)
(** Adds the bound symbol with the index to the symbol table. *)
val add : t -> bid_t -> bid_t option -> Flx_bsym.t -> unit

(** Remove a binding from the bound symbol table. *)
val remove : t -> bid_t -> unit

(* Order is: super, sub = coercion codomin, domin = parameter, arg *)
type coercion_t = (bid_t * bid_t) * bid_t

val add_supertype: t -> coercion_t -> unit
val is_supertype: t -> bid_t -> bid_t -> bool
val maybe_coercion: t -> bid_t -> bid_t -> bid_t option
val iter_coercions: t -> (coercion_t -> unit) -> unit
val fold_coercions: t -> ('a -> coercion_t -> 'a) -> 'a -> 'a

(* UNSAFE
(** Updates a bound symbol in place while preserving the child-parent
 * relationships. *)
val update : t -> bid_t -> Flx_bsym.t -> unit
*)

(* ENTRY MUTATORS -- DONT CHANGE TABLE STRUCTURE *)
(** Update a bound symbol's bbdcl in place. *)
val update_bbdcl : t -> bid_t -> Flx_bbdcl.t -> unit

(** Update all the bound function and procedure's bound exes. *)
val update_bexes : (Flx_bexe.t list -> Flx_bexe.t list) -> t -> unit

(* ACCESSORS *)

(** Returns how many items are in the bound symbol table. *)
val length : t -> int

(** Returns if the bound index is in the bound symbol table. *)
val mem : t -> bid_t -> bool

(** Searches the bound symbol table for the given symbol. *)
val find : t -> bid_t -> Flx_bsym.t

(** Searches the bound symbol table for the given symbol. *)
val find_with_parent :
  t ->
  bid_t ->
  bid_t option *  Flx_bsym.t

(** Searches the bound symbol table for the given symbol's parent. *)
val find_parent : t -> bid_t -> bid_t option

(** Searches the bound symbol table for the given symbol's children. *)
val find_children : t -> bid_t -> BidSet.t

(** Finds all the descendants of the given symbol. *)
val find_descendants: t -> bid_t -> BidSet.t

(** Searches the bound symbol table for the given symbol's id. *)
val find_id : t -> bid_t -> Flx_id.t

(** Searches the bound symbol table for the given symbol's source reference. *)
val find_sr : t -> bid_t -> Flx_srcref.t

(** Searches the bound symbol table for the given symbol's bbdcl. *)
val find_bbdcl : t -> bid_t -> Flx_bbdcl.t

(** Searches the bound symbol table for the given symbol's bparams. *)
val find_bparams : t -> bid_t -> Flx_bparams.t

(** Searches the bound symbol table for the given symbol's bvs. *)
val find_bvs : t -> bid_t -> Flx_kind.bvs_t

(** Copies the bound symbol table. *)
val copy : t -> t

(* UNSAFE
(** Set's a symbol's parent. *)
val set_parent : t -> bid_t -> bid_t option -> unit
*)

(** Iterate over all the items in the bound symbol table. *)
val iter :
  (bid_t -> bid_t option -> Flx_bsym.t -> unit) ->
  t ->
  unit

(** Fold over all the items in the bound symbol table. *)
val fold :
  (bid_t -> bid_t option -> Flx_bsym.t -> 'a -> 'a) ->
  t ->
  'a ->
  'a

(** Returns whether or not one symbol is a child of another. *)
val is_child: t -> bid_t -> bid_t -> bool

(** Returns whether or not one symbol is an ancestor of another. *)
val is_ancestor: t -> bid_t -> bid_t -> bool

(** Return if the bound symbol index is an identity function. *)
val is_identity : t -> bid_t -> bool

(** Return if the bound symbol index is a val or var. *)
val is_variable : t -> bid_t -> bool

(** Return if the bound symbol index is a global val or var. *)
val is_global_var : t -> bid_t -> bool

(** Return if the bound symbol index is an identity function. *)
val is_function : t -> bid_t -> bool

(** Assert that the bound symbol table is well formed. *)
val validate : string -> t -> unit
val validate_types: (Flx_btype.t -> unit) -> t -> unit

