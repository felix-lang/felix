open Flx_btype
open Flx_bbdcl

(* Count number of cases in variant *)
let cal_variant_cases bsym_table t =
  match t with
  | BTYP_void -> 0
  | BTYP_rptsum (BTYP_unitsum k,_) -> k
  | BTYP_rptsum _ -> failwith ("Number of cases of nonunitsum repeated sum not handled yet")

  | BTYP_sum ls -> List.length ls
  | BTYP_unitsum i -> i
  | BTYP_variant ls -> List.length ls
  | BTYP_inst (i,ts,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found -> assert false
    in
    begin match Flx_bsym.bbdcl bsym with
    (* special: we take the max of declared constructors and the maximum user defined index
     * It's not clear this is right, however the index actually stored in a packed pointer
     * is the user asssigned one so it has to fit. Note: we have to use the assigned value + 1,
     * since we're calculating a case count, an 0 .. n is n+1 cases. We're assuming non-negative
     * values here .. urrggg.. review this!
     *)
    | BBDCL_union (bvs,cts) -> List.fold_left (fun a (s,i,evs,t,_,gadt) -> max a (i+1)) (List.length cts) cts
    | x -> failwith 
        ("cal variant cases of non-variant nominal type " ^ 
          Flx_print.string_of_bbdcl bsym_table x i 
        )
    end
  | _ -> 
    print_endline ("Find number of cases of non-sum type " ^ Flx_print.sbt bsym_table t);
    assert false 

(* size of data type in machine words, 2 means 2 or more 
  We used to put unitsum in here too, the problem is size 1
  leads to packed representation, but that rep just bitors
  the case number and data together .. works for aligned
  pointers, doesn't work for integral data.
*)
let size t = match t with
  | BTYP_void -> -1
  | BTYP_tuple [] -> 0
  | BTYP_pointer _  (* this is WRONG won't work for char* at odd address *)
  | BTYP_function _
  | BTYP_cfunction _
    -> 1
  | _ -> 2

let cal_variant_maxarg bsym_table t =
  match t with
  | BTYP_void -> -1 (* special for void *)
  | BTYP_rptsum (_,t) -> size t
  | BTYP_sum ls -> List.fold_left (fun r t -> max r (size t)) 0 ls
  | BTYP_unitsum i -> 0
  | BTYP_variant ls -> List.fold_left (fun r (_,t) -> max r (size t)) 0 ls
  | BTYP_inst (i,ts,_) ->
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
      List.fold_left (fun r (_,_,evs,t,_,_) -> max r (size t)) 0  cts
    | _ -> assert false 
    end
  | _ -> assert false 

let isnullptr bsym_table t = match t with
  | BTYP_inst (i,_,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found -> assert false
    in
    begin match Flx_bsym.bbdcl bsym with
    (* nullptr rep: must be a union, with constant constructor first,
       and then some other constructor, but do NOT allow the second
       constructor to take a unit argument because a pointer to a unit
       is encoded as a NULL
    *)
    | BBDCL_union (bvs,[id1,0,[],BTYP_void,_,false; id2, 1, [],BTYP_tuple [],_,false]) -> false
    | BBDCL_union (bvs,[id1,0,[],BTYP_void,_,false; id2, 1, [],t2,_,false]) -> true

    | _ -> false
    end
  | _ -> false

let weird_unit bsym_table t = match t with
  | BTYP_inst (i,_,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found -> assert false
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (bvs,[id1,0,[],BTYP_void,_,_; id2, 1, [],BTYP_tuple [],_,_]) -> true
    | _ -> false
    end
  | _ -> false

let is_gadt bsym_table t = match t with
  | BTYP_inst (i,_,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found -> assert false
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (bvs,cps) -> 
      List.fold_left (fun acc (id,idx,evs,d,c,gadt) -> gadt || acc) false cps
    | _ -> false
    end
  | _ -> false



type variant_rep = 
  | VR_self       (* only one ctor so the union is just the argument *)
  | VR_int        (* all constant ctors, just need the index *) 
  | VR_nullptr    (* special case None or Some of T etc, use NULL for None, non-NULL ptr to T for Some *)
  | VR_packed     (* 4 or less ctors, use pointer with index in low 2 bits *)
  | VR_uctor      (* general form: int tag and pointer *)

(* Note: where a pointer to T would normally be used, if T is already a pointer to U,
   then store it directly instead of a pointer to it
*)

let string_of_variant_rep = function
  | VR_self -> "VR_self (DEPRECATED)"
  | VR_int -> "VR_int"
  | VR_nullptr -> "VR_nullptr"
  | VR_packed -> "VR_packed"
  | VR_uctor -> "VR_uctor"


let cal_variant_rep bsym_table t =
 (* IF NOT SURE WE COULD MAKE rptsum always use VR_uctor .. *)
  if is_gadt bsym_table t then VR_uctor else
  (* variant types universally use _uctor_ since they're open *)
  match t with BTYP_variant _ -> VR_uctor | _ ->
  if isnullptr bsym_table t then 
    VR_nullptr
  else if weird_unit bsym_table t then
    VR_packed
  else if Flx_btype.islinear_type bsym_table t then
    VR_int
  else
  let n = cal_variant_cases bsym_table t in
  let z = cal_variant_maxarg bsym_table t in
  let rep =
    match n,z with
    | -1,_ -> assert false
    | 1,_ -> VR_self                  (* only one case do drop variant *)
    | _,0 -> VR_int                  (* no arguments, just use an int *)
    | k,_ when k <= 4 -> VR_packed   (* At most 4 cases, encode caseno in point low bits *)
    | _,_ -> VR_uctor                (* Standard Uctor *)

  in 
  rep

