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

type offset_kind_t = [`Ptr of string | `Prim of string * Flx_btype.t]

let add_offset o inner =
  match inner with 
  | `Ptr i -> `Ptr (o ^ "+" ^ i) 
  | `Prim (i,t) -> `Prim (o ^ "+" ^ i, t)


let offsetof s e =
  "offsetof(" ^ s ^ "," ^ e ^ ")"


let rec get_offsets' syms bsym_table typ : offset_kind_t list =
  let tname = cpp_typename syms bsym_table typ in
  let t' = unfold "flx_cal_type_offsets: get_offsets" typ in
  match t' with
  | BTYP_ellipsis -> assert false
  | BBOOL _ -> assert false
  | BTYP_typeop _ -> assert false
  | BTYP_typeof _ -> assert false
  | BTYP_rev _ -> assert false
  | BTYP_uniq _ -> assert false
  | BTYP_borrowed _ -> assert false

  | BTYP_instancetype _ ->
    (* print_endline ("Cal_type_offsets: instancetype"); *)
    (* this shouldn't happen but somehow it does .. *)
    [`Ptr "0"]

  | BTYP_ptr (_,t,_) -> [`Ptr "0"]


  | BTYP_variant _ ->
    [`Ptr (offsetof tname "data")]
  
  (* compact linear *)
  | BTYP_tuple [] 
  | BTYP_compacttuple _
  | BTYP_compactarray _
  | BTYP_compactsum _
  | BTYP_compactrptsum _
  | BTYP_unitsum _ -> []


  (* need to fix the rule for optimisation here .. *)
  | BTYP_rptsum _ 
  | BTYP_sum _ ->
    begin match Flx_vrep.cal_variant_rep bsym_table t' with
    | Flx_vrep.VR_self -> assert false (* FIXME! *) 
    | Flx_vrep.VR_clt -> []
    | Flx_vrep.VR_int -> []
    | Flx_vrep.VR_nullptr -> [`Ptr "0"]
    | Flx_vrep.VR_packed -> [`Ptr "0"]
    | Flx_vrep.VR_uctor -> [`Ptr (offsetof tname "data")]
    end

  | BTYP_vinst _ -> assert false

  | BTYP_finst _  -> assert false
  | BTYP_inst (`Alias, i,ts,_) -> assert false
  | BTYP_inst (`Nominal, i,ts,_) ->
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
      | Flx_vrep.VR_nullptr -> [`Ptr "0"]
      | Flx_vrep.VR_packed -> [`Ptr "0"]
      | Flx_vrep.VR_uctor -> [`Ptr (offsetof tname "data")]
      end

    | BBDCL_struct (vs,idts) ->
      let varmap = mk_varmap (Flx_bsym.sr bsym) vs ts in
      let idts = map (fun (s,t) -> s,varmap_subst varmap t) idts in
      let n = ref 0 in
      let lst = ref [] in
      iter
      (fun (s,t) ->
        let prefix = offsetof tname (cid_of_flxid s) in
        iter
        (fun s -> lst := !lst @ [add_offset prefix  s])
        (get_offsets' syms bsym_table t)
      )
      idts
      ;
      !lst

    | BBDCL_external_type (_,type_quals,_,_) ->
       if mem `GC_pointer type_quals then [`Ptr "0"]
       else if has_scanner type_quals then [`Prim ("0",t')]
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
        let ss = si i ^ "*sizeof("^eltype^")" in
        fold_left
        (fun result s -> (add_offset ss s) :: result)
        result
        toffsets
      )
      []
      (nlist k)
    end

  (* NOTES. This type really does exist at run time. Consider an ObjC protocol A & B,
     them values of this type are ObjC class instances conforming to both A and B.
     To correctly dispatch methods in the back end we should in principle use a C union
     here with one component for A and one for B, and statically determine in Felix
     which "cast" to use.

    For now I am going to CHEAT and just assume that's the only case, so a void* representation
    will do. The "casting" is necessary if the SAME ObjC selector is in both protocols,
    then we have to pick which one we want by overloading. How do we do that? Well the CLIENT
    has the same problem: its ambiguous. So THEY have to use a coercion of the pointer themselves
    to either A or B, and we just propagate the coercion. The coercion throws out one of the
    methods and forces the other to be selected. 
  *)
  | BTYP_intersect _ -> 
    print_endline ("Flx_cal_type_offsets: HOT intersection type"); 
    [`Ptr "0"]

  | BTYP_union _ -> 
    print_endline ("Flx_cal_type_offsets: HOT union type"); 
    [`Ptr "0"]

  | BTYP_tuple args ->
    let n = ref 0 in
    let lst = ref [] in
    iter
    (fun t ->
      let prefix = offsetof tname ("mem_"^si !n) in
      iter
      (fun s -> lst := !lst @ [add_offset prefix s])
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
      let prefix = offsetof tname (cid_of_flxid s) in
      iter
      (fun s -> lst := !lst @ [add_offset prefix s])
      (get_offsets' syms bsym_table t)
    )
    es 
    ;
    !lst

  | BTYP_lineareffector _ 
  | BTYP_linearfunction _ -> [`Ptr "0"]
  | BTYP_effector _ 
  | BTYP_function _ -> [`Ptr "0"]
  | BTYP_cfunction _ -> []

  | BTYP_label -> [`Ptr "0"] (* see jump_address_t, target_frame at offset 0 *)

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

  | BTYP_in _
  | BTYP_type_var _
  | BTYP_type_apply _
  | BTYP_type_map _
  | BTYP_type_function _
  | BTYP_type_tuple _
  | BTYP_type_match _
  | BTYP_subtype_match _
  | BTYP_type_set_intersection _
  | BTYP_type_set_union _ -> assert false

let render_offset syms bsym_table new_table s =
  match s with 
    | `Ptr s -> "{" ^ s ^",nullptr}"
    | `Prim (s,t) -> 
      let index' = Flx_treg.find_type_index syms bsym_table t in
      Hashtbl.replace new_table t index';
      let shape = Flx_pgen.shape_of' true syms bsym_table (cpp_typename syms bsym_table) t in
      "{" ^ s ^", &"^shape^"/* "^sbt bsym_table t^"*/}"

let get_offsets syms bsym_table new_table typ =
  map  (render_offset syms bsym_table new_table ) (get_offsets' syms bsym_table typ)

