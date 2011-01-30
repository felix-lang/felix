open Flx_btype
open Flx_bbdcl

(* Count number of cases in variant *)
let cal_variant_cases bsym_table t =
  match t with
  | BTYP_void -> 0
  | BTYP_sum ls -> List.length ls
  | BTYP_unitsum i -> i
  | BTYP_variant ls -> List.length ls
  | BTYP_inst (i,ts) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found -> assert false
    in
    begin match Flx_bsym.bbdcl bsym with
    (* BUG: the user is allowed to assign the indexes, so we should look for the max index .. 
     * but I'm not sure we should be supporting this now: it's to support C enums
     *)
    | BBDCL_union (bvs,cts) -> List.length cts
    | _ -> assert false 
    end
  | _ -> assert false 

(* size of data type in machine words, 2 means 2 or more *)
let size t = match t with
  | BTYP_void -> -1
  | BTYP_tuple [] -> 0

  | BTYP_pointer _ 
  | BTYP_function _
  | BTYP_cfunction _
  | BTYP_unitsum _ 
    -> 1
  | _ -> 2

let cal_variant_maxarg bsym_table t =
  match t with
  | BTYP_void -> -1 (* special for void *)
  | BTYP_sum ls -> List.fold_left (fun r t -> max r (size t)) 0 ls
  | BTYP_unitsum i -> 0
  | BTYP_variant ls -> List.fold_left (fun r (_,t) -> max r (size t)) 0 ls
  | BTYP_inst (i,ts) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found -> assert false
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (bvs,cts) -> 
      (* Note hack: ignore type variables .. might come back to bite us
       * Means that a polymorphic variant might not have optimal size
       * if a type variable were instantiated with a small size, but
       * hopefully this will be consistent!
       *)
      List.fold_left (fun r (_,u,t) -> max r (size t)) 0  cts
    | _ -> assert false 
    end
  | _ -> assert false 

type variant_rep = VR_self | VR_int |  VR_packed | VR_uctor

let cal_variant_rep bsym_table t =
  let n = cal_variant_cases bsym_table t in
  let z = cal_variant_maxarg bsym_table t in
  match n,z with
  | -1,_ -> assert false
(* Remove this case temporarily because it is a bit tricky to implement *)
(*  | 1,_ -> VR_self *)                 (* only one case do drop variant *)
  | _,0 -> VR_int                  (* no arguments, just use an int *)
  | k,_ when k <= 4 -> VR_packed   (* At most 4 cases, encode caseno in point low bits *)
  | _,_ -> VR_uctor                (* Standard Uctor *)


