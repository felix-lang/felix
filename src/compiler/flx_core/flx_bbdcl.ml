open Format
open Flx_ast
open Flx_types
open Flx_format

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

(** Convert the bound declaration to a string. *)
let rec print f = function
  | BBDCL_module -> print_variant0 f "BBDCL_module"
  | BBDCL_function (props,bvs,ps,res,es) ->
      print_variant5 f "BBDCL_function"
        print_properties props
        print_bvs bvs
        print_bparams ps
        print_btype res
        (Flx_list.print print_bexe) es
  | BBDCL_procedure (props,bvs,ps,es) ->
      print_variant4 f "BBDCL_procedure"
        print_properties props
        print_bvs bvs
        print_bparams ps
        (Flx_list.print print_bexe) es
  | BBDCL_val (bvs,t) ->
      print_variant2 f "BBDCL_val" print_bvs bvs print_btype t
  | BBDCL_var (bvs,t) ->
      print_variant2 f "BBDCL_var" print_bvs bvs print_btype t
  | BBDCL_ref (bvs,t) ->
      print_variant2 f "BBDCL_ref" print_bvs bvs print_btype t
  | BBDCL_tmp (bvs,t) ->
      print_variant2 f "BBDCL_tmp" print_bvs bvs print_btype t
  | BBDCL_newtype (bvs,t) ->
      print_variant2 f "BBDCL_newtype" print_bvs bvs print_btype t
  | BBDCL_abs (bvs,quals,code,reqs) ->
      print_variant4 f "BBDCL_abs"
        print_bvs bvs
        (Flx_list.print print_btype_qual) quals
        print_code_spec code
        print_breqs reqs
  | BBDCL_const (props,bvs,t,code,reqs) ->
      print_variant5 f "BBDCL_const"
        print_properties props
        print_bvs bvs
        print_btype t
        print_code_spec code
        print_breqs reqs
  | BBDCL_fun (props,bvs,ps,rt,code,reqs,prec) ->
      print_variant7 f "BBDCL_fun"
        print_properties props
        print_bvs bvs
        print_btypes ps
        print_btype rt
        print_code_spec code
        print_breqs reqs
        print_string prec
  | BBDCL_callback (props,bvs,ps_cf,ps_c,k,rt,reqs,prec) ->
      print_variant8 f "BBDCL_callback"
        print_properties props
        print_bvs bvs
        print_btypes ps_cf
        print_btypes ps_c
        pp_print_int k
        print_btype rt
        print_breqs reqs
        print_string prec
  | BBDCL_proc (props,bvs,ps,code,reqs) ->
      print_variant5 f "BBDCL_proc"
        print_properties props
        print_bvs bvs
        print_btypes ps
        print_code_spec code
        print_breqs reqs
  | BBDCL_insert (bvs,code,ikind,reqs) ->
      let ikind =
        match ikind with
        | `Header -> "header"
        | `Body -> "body"
        | `Package -> "package"
      in
      print_variant4 f "BBDCL_insert"
        print_bvs bvs
        print_code_spec code
        pp_print_string ikind
        print_breqs reqs
  | BBDCL_union (bvs, cs) ->
      print_variant2 f "BBDCL_union"
        print_bvs bvs
        (Flx_list.print begin fun f (n,i,t) ->
          print_tuple3 f
            print_string n
            pp_print_int i
            print_btype t
        end) cs
  | BBDCL_struct (bvs, cs) ->
      print_variant2 f "BBDCL_struct"
        print_bvs bvs
        (Flx_list.print begin fun f (n,t) ->
          print_tuple2 f
            print_string n
            print_btype t
        end) cs
  | BBDCL_cstruct (bvs, cs) ->
      print_variant2 f "BBDCL_struct"
        print_bvs bvs
        (Flx_list.print begin fun f (n,t) ->
          print_tuple2 f
            print_string n
            print_btype t
        end) cs
  | BBDCL_typeclass (props,bvs) ->
      print_variant2 f "BBDCL_typeclass"
        print_properties props
        print_bvs bvs
  | BBDCL_instance (props,bvs,cons,bid,ts) ->
      print_variant5 f "BBDCL_instance"
        print_properties props
        print_bvs bvs
        print_btype cons
        print_bid bid
        print_btypes ts
  | BBDCL_nonconst_ctor (bvs,uidx,ut,ctor_idx,ctor_argt,evs,etraint) ->
      print_variant7 f "BBDCL_nonconst_ctor"
        print_bvs bvs
        print_bid uidx
        print_btype ut
        print_bid ctor_idx
        print_btype ctor_argt
        print_bvs evs
        print_btype etraint
