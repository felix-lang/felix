open Flx_util
open Flx_list
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bbdcl
open Flx_mtypes2
open Flx_name
open Flx_unify
open Flx_typing
open List
open Flx_print
open Flx_exceptions
open Flx_maps
open Flx_btype_subst

exception NestedFunctor of Flx_btype.t * string

(* BAD BAD this code is copied from flx_cal_type_shape .. *)
let has_user_scanner quals = 
  try 
    List.iter (fun q-> match q with | `Scanner cs -> raise Not_found | _ -> () ) quals; 
    false
  with Not_found -> true

let has_scanner quals = 
  has_user_scanner quals ||
  mem (`TypeTag "functor") quals 
 
 

let unitsum bsym_table t = 
  try Flx_btype.int_of_linear_type bsym_table t 
  with Invalid_int_of_unitsum -> -1

(* this code handles pointers in types 
 * it returns a list of strings which are C expressions for the
 * offsets of each pointer in the type.
 *)
let rec get_offsets' syms bsym_table typ : string list =
  let tname = cpp_typename syms bsym_table typ in
  let t' = unfold "flx_cal_type_offsets: get_offsets" typ in
  match t' with
  | BBOOL _ -> assert false
  | BTYP_typeop _ -> assert false
  | BTYP_typeof _ -> assert false
  | BTYP_hole -> assert false
  | BTYP_rev _ -> assert false
  | BTYP_uniq _ -> assert false
  | BTYP_ptr (_,t,_) -> ["0"]


  | BTYP_variant _ ->
    ["offsetof("^tname^",data)"]

  (* need to fix the rule for optimisation here .. *)
  | BTYP_rptsum _ 
  | BTYP_sum _ ->
    begin match Flx_vrep.cal_variant_rep bsym_table t' with
    | Flx_vrep.VR_self -> assert false (* FIXME! *) 
    | Flx_vrep.VR_clt -> []
    | Flx_vrep.VR_int -> []
    | Flx_vrep.VR_nullptr -> ["0"]
    | Flx_vrep.VR_packed -> ["0"]
    | Flx_vrep.VR_uctor -> ["offsetof("^tname^",data)"]
    end

  | BTYP_vinst _ -> assert false

  | BTYP_inst (i,ts,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i
      with Not_found -> failwith
        ("get_offsets'] can't find index " ^ string_of_bid i)
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (vs, [id,n,[],t',_,false]) ->  assert false
(*    
      ;
      let t'' = tsubst (Flx_bsym.sr bsym) vs ts t' in
      get_offsets' syms bsym_table t''
*)
    | BBDCL_union (vs,idts) ->
(*
      let varmap = mk_varmap vs ts in
      let cpts = map (fun (_,_,t) -> varmap_subst varmap t) idts in
      if all_voids cpts then []
      else ["offsetof("^tname^",data)"]
*)
      begin match Flx_vrep.cal_variant_rep bsym_table t' with
      | Flx_vrep.VR_self -> assert false (* FIXME! *)
      | Flx_vrep.VR_clt -> []
      | Flx_vrep.VR_int -> []
      | Flx_vrep.VR_nullptr -> ["0"]
      | Flx_vrep.VR_packed -> ["0"]
      | Flx_vrep.VR_uctor -> ["offsetof("^tname^",data)"]
      end

    | BBDCL_struct (vs,idts) ->
      let varmap = mk_varmap (Flx_bsym.sr bsym) vs ts in
      let idts = map (fun (s,t) -> s,varmap_subst varmap t) idts in
      let n = ref 0 in
      let lst = ref [] in
      iter
      (fun (s,t) ->
        let prefix =
          "offsetof("^tname^","^cid_of_flxid s^")+"
        in
        iter
        (fun s -> lst := !lst @ [prefix ^ s])
        (get_offsets' syms bsym_table t)
      )
      idts
      ;
      !lst

    | BBDCL_external_type (_,type_quals,_,_) ->
       if mem `GC_pointer type_quals then ["0"]
       else if has_scanner type_quals then raise (NestedFunctor (t', sbt bsym_table t'))
       else []

    | _ -> []
    end

  | BTYP_array (t,u) when unitsum bsym_table u = 0 -> []
  | BTYP_array (t,u) when unitsum bsym_table u > 0 -> 
    let k = unitsum bsym_table u in
    let toffsets = get_offsets' syms bsym_table t in
    if toffsets = [] then [] else
    if k> 100 then
      failwith ("[get_offsets] Too many elements in array for shape, type " ^ sbt bsym_table t')
    else begin
      let eltype = cpp_typename syms bsym_table t in
      fold_left
      (fun result i ->
        let ss = "+" ^ si i ^ "*sizeof("^eltype^")" in
        fold_left
        (fun result s -> (s ^ ss) :: result)
        result
        toffsets
      )
      []
      (nlist k)
    end

  | BTYP_tuple args ->
    let n = ref 0 in
    let lst = ref [] in
    iter
    (fun t ->
      let prefix =
        "offsetof("^tname^",mem_"^si !n^")+"
      in
      iter
      (fun s -> lst := !lst @ [prefix ^ s])
      (get_offsets' syms bsym_table t)
      ;
      incr n
    )
    args
    ;
    !lst

  | BTYP_record (es) ->
    let lst = ref [] in
    iter
    (fun (s,t) ->
      let prefix =
        "offsetof("^tname^","^cid_of_flxid s^")+"
      in
      iter
      (fun s -> lst := !lst @ [prefix ^ s])
      (get_offsets' syms bsym_table t)
    )
    es 
    ;
    !lst

  | BTYP_effector _ 
  | BTYP_function _ -> ["0"]
  | BTYP_cfunction _ -> []

  | BTYP_unitsum _ -> []

  | BTYP_label -> ["0"] (* see jump_address_t, target_frame at offset 0 *)

  (* this is a lie .. it does, namely a plain C union *)
  | BTYP_type_set _
    -> failwith "[ogen] Type set has no representation"

  | BTYP_polyrecord _ 
  | BTYP_polyvariant _ 
  | BTYP_tuple_cons _ 
  | BTYP_tuple_snoc _ 
  | BTYP_none 


  | BTYP_array _
  | BTYP_fix _
  | BTYP_void

  | BTYP_type_var _
  | BTYP_type_apply _
  | BTYP_type_map _
  | BTYP_type_function _
  | BTYP_type_tuple _
  | BTYP_type_match _
  | BTYP_subtype_match _
  | BTYP_type_set_intersection _
  | BTYP_type_set_union _ -> assert false

let get_offsets syms bsym_table typ =
  map (fun s -> s^",") (get_offsets' syms bsym_table typ)

(**********************************************************************)

