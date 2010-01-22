open Format
open Flx_ast
open Flx_types

(** Bound declarations. *)
type t =
  | BBDCL_module
  | BBDCL_function of   property_t list * bvs_t * bparams_t * btypecode_t * bexe_t list
  | BBDCL_procedure of  property_t list * bvs_t * bparams_t * bexe_t list
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
let get_ts = function
  | BBDCL_instance (_, _, _, _, ts) -> ts
  | _ -> []

(** Extract the bound type variables of a bound declaration. *)
let get_bvs = function
  | BBDCL_module -> []
  | BBDCL_function (_, bvs, _, _, _) -> bvs
  | BBDCL_procedure (_, bvs, _, _) -> bvs
  | BBDCL_val (bvs, _) -> bvs
  | BBDCL_var (bvs, _) -> bvs
  | BBDCL_ref (bvs, _) -> bvs
  | BBDCL_tmp (bvs, _) -> bvs
  | BBDCL_newtype (bvs, _) -> bvs
  | BBDCL_abs (bvs, _, _, _) -> bvs
  | BBDCL_const (_, bvs, _, _, _) -> bvs
  | BBDCL_fun (_, bvs, _, _, _, _, _) -> bvs
  | BBDCL_callback (_, bvs, _, _, _, _, _, _) -> bvs
  | BBDCL_proc (_, bvs, _, _, _) -> bvs
  | BBDCL_insert (bvs, _, _, _) -> bvs
  | BBDCL_union (bvs, _) -> bvs
  | BBDCL_struct (bvs, _) -> bvs
  | BBDCL_cstruct (bvs, _) -> bvs
  | BBDCL_typeclass (_, bvs) -> bvs
  | BBDCL_instance (_, bvs, _, _, _) -> bvs
  | BBDCL_nonconst_ctor (bvs, _, _, _, _, _, _) -> bvs
