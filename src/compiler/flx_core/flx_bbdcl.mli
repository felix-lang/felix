open Flx_ast
open Flx_types

type btype_qual_t = [
  | Flx_ast.base_type_qual_t
  | `Bound_needs_shape of Flx_btype.t
]

type breqs_t = (Flx_types.bid_t * Flx_btype.t list) list

(** Bound declarations. *)
type t =
  | BBDCL_module
  | BBDCL_function of   property_t list * bvs_t * Flx_bparams.t * Flx_btype.t * Flx_bexe.t list
  | BBDCL_procedure of  property_t list * bvs_t * Flx_bparams.t * Flx_bexe.t list
  | BBDCL_val of        bvs_t * Flx_btype.t
  | BBDCL_var of        bvs_t * Flx_btype.t
  | BBDCL_ref of        bvs_t * Flx_btype.t
  | BBDCL_tmp of        bvs_t * Flx_btype.t

  (* binding structures [prolog] *)
  | BBDCL_newtype of    bvs_t * Flx_btype.t
  | BBDCL_abs of        bvs_t * btype_qual_t list * code_spec_t * breqs_t
  | BBDCL_const of      property_t list * bvs_t * Flx_btype.t * code_spec_t * breqs_t
  | BBDCL_fun of        property_t list * bvs_t * Flx_btype.t list * Flx_btype.t * code_spec_t  * breqs_t * prec_t
  | BBDCL_callback of   property_t list * bvs_t * Flx_btype.t list * Flx_btype.t list * int * Flx_btype.t * breqs_t * prec_t
  | BBDCL_proc of       property_t list * bvs_t * Flx_btype.t list * code_spec_t  * breqs_t
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

(** Extract the parameters of a bound declaration. *)
val get_bparams : t -> Flx_bparams.t

(** Extract the types of a bound declaration. *)
val get_ts : t -> Flx_btype.t list

(** Extract the bound type variables of a bound declaration. *)
val get_bvs : t -> bvs_t

(** Prints a bbdcl to a formatter. *)
val print : Format.formatter -> t -> unit
