open Flx_btype
open Flx_bbdcl
open Flx_cexpr
open Flx_ctypes
open Flx_vrep

let si = string_of_int

let check_case_index bsym_table t i =
  let n = cal_variant_cases bsym_table t in
  assert (0 <= i && i < n)

(* get the index of a variant value for purpose of matching cases *)
let gen_get_case_index ge bsym_table e: cexpr_t  =
  let _,t = e in
  match cal_variant_rep bsym_table t with
  | VR_self -> ce_atom "0"
  | VR_int -> ge e
  | VR_packed -> ce_call (ce_atom "FLX_VI") [ge e]
  | VR_uctor -> ce_dot (ge e) "variant"

(* helper to get the argument type of a non-constant variant constructor *)
let cal_case_type bsym_table n t : Flx_btype.t =
  match t with
  | BTYP_unitsum _ -> Flx_btype.btyp_tuple []
  | BTYP_sum ls -> List.nth ls n
  | BTYP_variant ls -> let (_,ct) = List.nth ls n in ct
  | BTYP_inst (i,ts) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found -> assert false
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (bvs,cts) -> 
      let _,_,ct = List.nth cts n in
      let ct = Flx_unify.tsubst bvs ts ct in (* eliminate type variables *)
      ct
    | _ -> assert false
    end
  | _ -> assert false

(* get the argument of a non-constant variant constructor. To be used in a match
 *  when the case index has been found.
 *)
let gen_get_case_arg ge tn bsym_table n (e:Flx_bexpr.t) : cexpr_t =
  let x,ut = e in
  let ct = cal_case_type bsym_table n ut in
  let cast = tn ct in
  match cal_variant_rep bsym_table ut with
  | VR_self -> ge e
  | VR_int -> assert false
  | VR_packed ->
    begin match size ct with
    | 0 -> assert false
    | 1 -> ce_cast cast (ce_call (ce_atom "FLX_VP") [ge e])
    | _ -> ce_prefix "*" (ce_cast (cast^"*") (ce_call (ce_atom "FLX_VP") [ge e]))
    end

  | VR_uctor ->
    begin match size ct with
    | 0 -> assert false
    | 1 -> ce_cast cast (ce_dot (ge e) "data") 
    | _ -> ce_prefix "*" (ce_cast (cast^"*") (ce_dot (ge e) "data"))
    end

(* Value constructor for constant (argumentless) variant constructor case *)
let gen_make_const_ctor bsym_table e : cexpr_t =
  let x,ut = e in
  let v,ut' = 
    match x with 
    | Flx_bexpr.BEXPR_case (v,ut) -> v,ut 
    | Flx_bexpr.BEXPR_name (i,ts) ->
      begin 
        try 
          let bsym = Flx_bsym_table.find bsym_table i in
          match Flx_bsym.bbdcl bsym with
          | BBDCL_const_ctor (vs,uidx,udt, ctor_idx, evs, etraint) ->
            let t = Flx_unify.tsubst vs ts udt in
            ctor_idx,t 
          | _ -> assert false
        with Not_found -> assert false
      end

    | _ -> assert (false) 
  in
  assert (ut = ut'); (* original code did an unfold here .. *)
  check_case_index bsym_table ut v;
  match cal_variant_rep bsym_table ut with
  | VR_self -> assert false (* will fail if there's one trivial case! FIX! Felix should elide completely *)
  | VR_int -> ce_atom (si v)
  | VR_packed -> ce_cast "void*" (ce_atom (si v))
  | VR_uctor -> ce_atom ("::flx::rtl::_uctor_(" ^ si v ^ ",0)") 

(* Helper function to make suitable argument for non-constant variant constructor *)
let gen_make_ctor_arg ge tn syms bsym_table a : cexpr_t =
  let _,ct = a in
  match size ct with
  | 0 -> ce_atom "0"                (* NULL: for unit tuple *)
  | 1 -> ge a                       (* small value goes right into data slot *)
  | _ ->                            (* make a copy on the heap and return pointer *)
    let ctt = tn ct in
    let ptrmap = Flx_pgen.shape_of syms bsym_table tn ct in
    ce_new [ce_atom "*PTF gcp"; ce_atom ptrmap; ce_atom "true"] ctt [ge a]

(* Value constructor for non-constant (argumentful) variant constructor case *)
let gen_make_nonconst_ctor ge tn syms bsym_table ut cidx ct a : cexpr_t =
  match cal_variant_rep bsym_table ut with
  | VR_self -> ge a
  | VR_int -> assert false
  | VR_packed -> 
    let arg = gen_make_ctor_arg ge tn syms bsym_table a in
    ce_call (ce_atom "FLX_VR") [ce_atom (si cidx); arg]

  | VR_uctor ->  
    let arg = gen_make_ctor_arg ge tn syms bsym_table a in
    ce_call (ce_atom "::flx::rtl::_uctor_") [ce_atom (si cidx); arg] 

