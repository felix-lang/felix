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

(* this code handles pointers in types 
 * it returns a list of strings which are C expressions for the
 * offsets of each pointer in the type.
 *)
let rec get_offsets' syms bsym_table typ : string list =
  let tname = cpp_typename syms bsym_table typ in
  let t' = unfold typ in
  match t' with
  | BTYP_none -> assert false

  | BTYP_pointer t -> ["0"]

  (* need to fix the rule for optimisation here .. *)
  | BTYP_sum _
  | BTYP_variant _ ->
    begin match Flx_vrep.cal_variant_rep bsym_table t' with
    | Flx_vrep.VR_self -> assert false (* FIXME! *) 
    | Flx_vrep.VR_int -> []
    | Flx_vrep.VR_packed -> ["0"]
    | Flx_vrep.VR_uctor -> ["offsetof("^tname^",data)"]
    end

  | BTYP_inst (i,ts) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i
      with Not_found -> failwith
        ("get_offsets'] can't find index " ^ string_of_bid i)
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (vs,idts) ->
(*
      let varmap = mk_varmap vs ts in
      let cpts = map (fun (_,_,t) -> varmap_subst varmap t) idts in
      if all_voids cpts then []
      else ["offsetof("^tname^",data)"]
*)
      begin match Flx_vrep.cal_variant_rep bsym_table t' with
      | Flx_vrep.VR_self -> assert false (* FIXME! *)
      | Flx_vrep.VR_int -> []
      | Flx_vrep.VR_packed -> ["0"]
      | Flx_vrep.VR_uctor -> ["offsetof("^tname^",data)"]
      end

    | BBDCL_struct (vs,idts) ->
      let varmap = mk_varmap vs ts in
      let n = ref 0 in
      let cpts = map (fun (s,t) -> s,varmap_subst varmap t) idts in
      let lst = ref [] in
      iter
      (fun (s,t) ->
        let prefix =
          "offsetof("^tname^","^s^")+"
        in
        iter
        (fun s -> lst := !lst @ [prefix ^ s])
        (get_offsets' syms bsym_table t)
      )
      cpts
      ;
      !lst

    | BBDCL_external_type (_,type_quals,_,_)
       when mem `GC_pointer type_quals -> ["0"]

    | _ -> []
    end

  | BTYP_array (t,BTYP_void) ->  []
  | BTYP_array (t,BTYP_unitsum k) ->
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

  | BTYP_record (_,args) ->
    let lst = ref [] in
    iter
    (fun (s,t) ->
      let prefix =
        "offsetof("^tname^","^s^")+"
      in
      iter
      (fun s -> lst := !lst @ [prefix ^ s])
      (get_offsets' syms bsym_table t)
    )
    args
    ;
    !lst

  | BTYP_function _ -> ["0"]
  | BTYP_cfunction _ -> []

  | BTYP_unitsum _ -> []

  | BTYP_intersect _
    -> failwith "[ogen] Type intersection has no representation"

  (* this is a lie .. it does, namely a plain C union *)
  | BTYP_type_set _
    -> failwith "[ogen] Type set has no representation"

  | BTYP_array _
  | BTYP_fix _
  | BTYP_void

  | BTYP_type  _
  | BTYP_type_var _
  | BTYP_type_apply _
  | BTYP_type_function _
  | BTYP_type_tuple _
  | BTYP_type_match _
  | BTYP_type_set_intersection _
  | BTYP_type_set_union _ -> assert false

let get_offsets syms bsym_table typ =
  map (fun s -> s^",") (get_offsets' syms bsym_table typ)


