open Flx_ast
open Flx_types

type btype_qual_t = [
  | Flx_ast.base_type_qual_t
  | `Bound_needs_shape of Flx_btype.t
]

type breqs_t = (Flx_types.bid_t * Flx_btype.t list) list

(** Used to represent all the different value types. *)
type value_kind_t = [`Val | `Var | `Ref | `Tmp]

(** Bound declarations. *)
type t = private
  | BBDCL_invalid
  | BBDCL_module
  | BBDCL_function of   property_t list * bvs_t * Flx_bparams.t * Flx_btype.t * Flx_bexe.t list
  | BBDCL_val of        bvs_t * Flx_btype.t * value_kind_t

  (* binding structures [prolog] *)
  | BBDCL_newtype of    bvs_t * Flx_btype.t
  | BBDCL_abs of        bvs_t * btype_qual_t list * code_spec_t * breqs_t
  | BBDCL_const of      property_t list * bvs_t * Flx_btype.t * code_spec_t * breqs_t
  | BBDCL_external_fun of
                        property_t list * bvs_t * Flx_btype.t list *
                        Flx_btype.t * code_spec_t  * breqs_t * prec_t
  | BBDCL_callback of   property_t list * bvs_t * Flx_btype.t list * Flx_btype.t list * int * Flx_btype.t * breqs_t * prec_t
  | BBDCL_insert of     bvs_t * code_spec_t * ikind_t * breqs_t

  | BBDCL_union of      bvs_t * (id_t * int * Flx_btype.t) list
  | BBDCL_struct of     bvs_t * (id_t * Flx_btype.t) list
  | BBDCL_cstruct of    bvs_t * (id_t * Flx_btype.t) list
  | BBDCL_typeclass of  property_t list * bvs_t
  | BBDCL_instance of   property_t list *
                        bvs_t *
                        Flx_btype.t (* constraint *) *
                        bid_t *
                        Flx_btype.t list
  | BBDCL_nonconst_ctor of bvs_t * bid_t * Flx_btype.t * int * Flx_btype.t *
                        bvs_t * Flx_btype.t (* existentials and constraint for GADTs *)
  | BBDCL_axiom
  | BBDCL_lemma
  | BBDCL_reduce

(* -------------------------------------------------------------------------- *)

val bbdcl_invalid : unit -> t
val bbdcl_module : unit -> t
val bbdcl_function : property_t list * bvs_t * Flx_bparams.t * Flx_btype.t * Flx_bexe.t list -> t
val bbdcl_val : bvs_t * Flx_btype.t * value_kind_t -> t
val bbdcl_newtype : bvs_t * Flx_btype.t -> t
val bbdcl_abs : bvs_t * btype_qual_t list * code_spec_t * breqs_t -> t
val bbdcl_const : property_t list * bvs_t * Flx_btype.t * code_spec_t * breqs_t -> t
val bbdcl_external_fun : property_t list * bvs_t * Flx_btype.t list * Flx_btype.t * code_spec_t  * breqs_t * prec_t -> t
val bbdcl_callback : property_t list * bvs_t * Flx_btype.t list * Flx_btype.t list * int * Flx_btype.t * breqs_t * prec_t -> t
val bbdcl_insert : bvs_t * code_spec_t * ikind_t * breqs_t -> t
val bbdcl_union : bvs_t * (id_t * int * Flx_btype.t) list -> t
val bbdcl_struct : bvs_t * (id_t * Flx_btype.t) list -> t
val bbdcl_cstruct : bvs_t * (id_t * Flx_btype.t) list -> t
val bbdcl_typeclass : property_t list * bvs_t -> t
val bbdcl_instance : property_t list * bvs_t * Flx_btype.t * bid_t * Flx_btype.t list -> t
val bbdcl_nonconst_ctor : bvs_t * bid_t * Flx_btype.t * int * Flx_btype.t * bvs_t * Flx_btype.t -> t
val bbdcl_axiom : unit -> t
val bbdcl_reduce : unit -> t
val bbdcl_lemma : unit -> t

(* -------------------------------------------------------------------------- *)

(** Extract the parameters of a bound declaration. *)
val get_bparams : t -> Flx_bparams.t

(** Extract the types of a bound declaration. *)
val get_ts : t -> Flx_btype.t list

(** Extract the bound type variables of a bound declaration. *)
val get_bvs : t -> bvs_t

(* -------------------------------------------------------------------------- *)

(** Return whether or not the bound declaration is valid. *)
val is_valid : t -> bool

(* -------------------------------------------------------------------------- *)

(** Recursively iterate over each bound declaration and call the function on
 * it. *)
val iter :
  ?f_bid:(Flx_types.bid_t -> unit) ->
  ?f_btype:(Flx_btype.t -> unit) ->
  ?f_bexpr:(Flx_bexpr.t -> unit) ->
  ?f_bexe:(Flx_bexe.t -> unit) ->
  t ->
  unit

(** Recursively iterate over each bound declaration and transform it with the
 * function. *)
val map :
  ?f_bid:(Flx_types.bid_t -> Flx_types.bid_t) ->
  ?f_btype:(Flx_btype.t -> Flx_btype.t) ->
  ?f_bexpr:(Flx_bexpr.t -> Flx_bexpr.t) ->
  ?f_bexe:(Flx_bexe.t -> Flx_bexe.t) ->
  t ->
  t

(* -------------------------------------------------------------------------- *)

(** Calls the function over every bid inside the bound declaration. *)
val iter_uses : (Flx_types.bid_t -> unit) -> t -> unit

(* -------------------------------------------------------------------------- *)

(** Prints a bbdcl to a formatter. *)
val print : Format.formatter -> t -> unit
