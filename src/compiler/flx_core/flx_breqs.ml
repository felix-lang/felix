open Flx_btype
open Flx_bbdcl

let rec strip_req bsym_table recstop (rid,ts) : breqs_t =
  if List.mem rid recstop then [rid,ts] else
  let recstop = rid :: recstop in
  match ts with
  | [] -> (* monomorphic *)
    let bsym = Flx_bsym_table.find bsym_table rid in
    let bbdcl = Flx_bsym.bbdcl bsym in
    begin match bbdcl with
    | BBDCL_external_code  ([],cs,ik,breqs) when Flx_code_spec.isempty cs ->
      strip_reqs' bsym_table recstop breqs   

    | BBDCL_external_code  _ -> [rid,ts]
    | _ -> failwith "Unexpected requirement on non-float"
    end
  | _ -> [rid,ts] (* polymorphic reqs unchanged *)
 
and strip_reqs' bsym_table recstop (rs:breqs_t):breqs_t =
  List.fold_left Flx_list.uniq_cat [] (List.map (strip_req bsym_table recstop) rs)

let strip_reqs bsym_table rs = strip_reqs' bsym_table [] rs

let strip_bbdcl bsym_table bbdcl =
  let f_breqs rs = strip_reqs bsym_table rs in
  match bbdcl with
  | BBDCL_external_type (a,b,c,breqs) ->
      let breqs = f_breqs breqs in
      bbdcl_external_type (a,b,c,breqs)

  | BBDCL_external_const (a,b,c,d,breqs) ->
      let breqs = f_breqs breqs in
      bbdcl_external_const (a,b,c,d,breqs)

  | BBDCL_external_fun (a,b,c,d,breqs,e,f) ->
      let breqs = f_breqs breqs in
      bbdcl_external_fun (a,b,c,d,breqs,e,f)

  | BBDCL_external_code (a,b,c,breqs) ->
      let breqs = f_breqs breqs in
      bbdcl_external_code (a,b,c,breqs)

  | BBDCL_cstruct (a,b,breqs) ->
      let breqs = f_breqs breqs in
       bbdcl_cstruct (a,b,breqs)

  | _ -> bbdcl

let simplify_reqs bsym_table =
  Flx_bsym_table.iter (fun bid parent bsym -> let bbdcl = Flx_bsym.bbdcl bsym in
     let bbdcl = strip_bbdcl bsym_table bbdcl in
     Flx_bsym_table.update_bbdcl bsym_table bid bbdcl
  ) 
  bsym_table 

