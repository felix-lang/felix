open Flx_btype

(* EVERYTHING is a plain old data type, except primitives
   which are not declared as such, non-pod primitives
   require a destructor. Note function types are pod because
   they're represented by pointers.
*)
let rec is_pod bsym_table t =
  let is_pod t = is_pod bsym_table t in
  match t with
  | BTYP_typeop _ -> assert false
  | BTYP_hole -> assert false
  | BTYP_uniq _ -> assert false

  | BTYP_label
  | BTYP_unitsum _ 
  | BTYP_sum _ 
  | BTYP_rptsum _ 
  | BTYP_ptr _
  | BTYP_function _
  | BTYP_cfunction _
  | BTYP_variant _ -> true
  | BTYP_tuple cps -> List.fold_left (fun acc t -> acc && is_pod t) true cps 
  | BTYP_record (cps) -> List.fold_left (fun acc (_,t) -> acc && is_pod t) true cps 
  | BTYP_array (t,_) -> is_pod t
  | BTYP_vinst (k,ts,_) -> assert false

  | BTYP_inst (k,ts,_) ->
    let bsym = Flx_bsym_table.find bsym_table k in
    let bbdcl = Flx_bsym.bbdcl bsym in
  begin match Flx_bsym_table.find_bbdcl bsym_table k with
    | BBDCL_union _ -> true
    | BBDCL_external_type (_,quals,_,_) -> List.mem `Pod quals
    | BBDCL_struct (vs,idts) -> 
      let varmap = Flx_btype_subst.mk_varmap (Flx_bsym.sr bsym) vs ts in
      let idts =  List.map (fun (s,t) -> s,Flx_btype_subst.varmap_subst varmap t) idts in
       List.fold_left (fun acc (_,t) -> acc && is_pod t) true idts
    | BBDCL_cstruct _ -> false
    | _ -> failwith ("[flx_cal_type_offsets: is_pod] Unexpected nominal type " ^ Flx_print.sbt bsym_table t)
  end
  | _ -> failwith ("[flx_cal_type_offsets: is_pod] Unexpected structural type " ^ Flx_print.sbt bsym_table t)
 

