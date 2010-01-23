open Flx_ast
open Flx_types

(** Bound declarations. *)
type t =
  | BBDCL_module
  | BBDCL_function of   property_t list * bvs_t * bparams_t * btypecode_t * Flx_bexe.t list
  | BBDCL_procedure of  property_t list * bvs_t * bparams_t * Flx_bexe.t list
  | BBDCL_val of        bvs_t * btypecode_t
  | BBDCL_var of        bvs_t * btypecode_t
  | BBDCL_ref of        bvs_t * btypecode_t
  | BBDCL_tmp of        bvs_t * btypecode_t

  (* binding structures [prolog] *)
  | BBDCL_newtype of    bvs_t * btypecode_t
  | BBDCL_abs of        bvs_t * btype_qual_t list * code_spec_t * breqs_t
  | BBDCL_const of      property_t list * bvs_t * btypecode_t * code_spec_t * breqs_t
  | BBDCL_fun of        property_t list * bvs_t * btypecode_t list * btypecode_t * code_spec_t  * breqs_t * prec_t
  | BBDCL_callback of   property_t list * bvs_t * btypecode_t list * btypecode_t list * int * btypecode_t * breqs_t * prec_t
  | BBDCL_proc of       property_t list * bvs_t * btypecode_t list * code_spec_t  * breqs_t
  | BBDCL_insert of     bvs_t * code_spec_t * ikind_t * breqs_t

  | BBDCL_union of      bvs_t * (id_t * int * btypecode_t) list
  | BBDCL_struct of     bvs_t * (id_t * btypecode_t) list
  | BBDCL_cstruct of    bvs_t * (id_t * btypecode_t) list
  | BBDCL_typeclass of  property_t list * bvs_t
  | BBDCL_instance of   property_t list *
                        bvs_t *
                        btypecode_t (* constraint *) *
                        bid_t *
                        btypecode_t list
  | BBDCL_nonconst_ctor of bvs_t * bid_t * btypecode_t * int * btypecode_t *
                         bvs_t * btypecode_t (* existentials and constraint for GADTs *)

(** Extract the types of a bound declaration. *)
val get_ts : t -> btypecode_t list

(** Extract the bound type variables of a bound declaration. *)
val get_bvs : t -> bvs_t

(** Prints a bbdcl to a formatter. *)
val print : Format.formatter -> t -> unit
