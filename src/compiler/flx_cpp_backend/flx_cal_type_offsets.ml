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

(* EVERYTHING is a plain old data type, except primitives
   which are not declared as such, non-pod primitives
   require a destructor. Note function types are pod because
   they're represented by pointers.
*)
let rec is_pod bsym_table t =
  let is_pod t = is_pod bsym_table t in
  match t with
  | BTYP_hole -> assert false
  | BTYP_uniq _ -> assert false
  | BTYP_rref _ -> assert false
  | BTYP_wref _ -> assert false

  | BTYP_label
  | BTYP_unitsum _ 
  | BTYP_sum _ 
  | BTYP_rptsum _ 
  | BTYP_pointer _
  | BTYP_cltpointer _
  | BTYP_cltrref _
  | BTYP_cltwref _
  | BTYP_function _
  | BTYP_cfunction _
  | BTYP_variant _ -> true
  | BTYP_tuple cps ->fold_left (fun acc t -> acc && is_pod t) true cps 
  | BTYP_record (cps) ->fold_left (fun acc (_,t) -> acc && is_pod t) true cps 
  | BTYP_array (t,_) -> is_pod t
  | BTYP_vinst (k,ts,_) -> assert false

  | BTYP_inst (k,ts,_) ->
    let bsym = Flx_bsym_table.find bsym_table k in
    let bbdcl = Flx_bsym.bbdcl bsym in
  begin match Flx_bsym_table.find_bbdcl bsym_table k with
    | BBDCL_union _ -> true
    | BBDCL_external_type (_,quals,_,_) -> mem `Pod quals
    | BBDCL_struct (vs,idts) -> 
      let varmap = mk_varmap (Flx_bsym.sr bsym) vs ts in
      let idts = map (fun (s,t) -> s,varmap_subst varmap t) idts in
      fold_left (fun acc (_,t) -> acc && is_pod t) true idts
    | BBDCL_cstruct _ -> false
    | _ -> failwith ("[flx_cal_type_offsets: is_pod] Unexpected nominal type " ^ sbt bsym_table t)
  end
  | _ -> failwith ("[flx_cal_type_offsets: is_pod] Unexpected structural type " ^ sbt bsym_table t)
 

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
  | BTYP_typeof _ -> assert false
  | BTYP_hole -> assert false
  | BTYP_rev _ -> assert false
  | BTYP_uniq _ -> assert false
  | BTYP_rref _ -> assert false
  | BTYP_wref _ -> assert false
  | BTYP_pointer t -> ["0"]

  | BTYP_cltpointer _
  | BTYP_cltrref _
  | BTYP_cltwref _ -> assert false (* RTL rep *)


  | BTYP_variant _ ->
    ["offsetof("^tname^",data)"]

  (* need to fix the rule for optimisation here .. *)
  | BTYP_rptsum _ 
  | BTYP_sum _ ->
    begin match Flx_vrep.cal_variant_rep bsym_table t' with
    | Flx_vrep.VR_self -> assert false (* FIXME! *) 
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

    | BBDCL_external_type (_,type_quals,_,_)
       when mem `GC_pointer type_quals -> ["0"]

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
  | BTYP_intersect _
    -> failwith "[ogen] Type intersection has no representation"

  | BTYP_union _
    -> failwith "[ogen] Type union has no representation"

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

module CS = Flx_code_spec
exception Scanner of CS.t

let rec get_encoder' syms bsym_table p typ : string list =
  let tname = cpp_typename syms bsym_table typ in
  let t' = unfold "flx_cal_type_offsets: encoder" typ in
  if is_pod bsym_table typ
  then
    ["b+=::flx::gc::generic::blit("^p^",sizeof("^tname^")); // pod"]
  else match t' with
  | BTYP_vinst _ -> assert false
  | BTYP_inst (i,ts,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i
      with Not_found -> failwith
        ("get_encoder'] can't find index " ^ string_of_bid i)
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (vs,idts) ->
      ["b+=::flx::gc::generic::blit("^p^",sizeof("^tname^")); // union"]

    | BBDCL_cstruct (vs, idts,_) (* this is NOT really correct ... *)
    | BBDCL_struct (vs,idts) ->
      let varmap = mk_varmap (Flx_bsym.sr bsym) vs ts in
      let idts = map (fun (s,t) -> s,varmap_subst varmap t) idts in
      let n = ref 0 in
      "//Struct" ::
      List.concat ( List.map 
      (fun (fld,t) ->
        let s= "offsetof("^tname^","^cid_of_flxid fld^")" in
        (get_encoder' syms bsym_table (p^"+"^s) t)
      )
      idts
      )

    | BBDCL_external_type (_,quals,_,_) ->
      let encoder = 
         try 
          List.iter (fun q-> match q with | `Encoder cs -> raise (Scanner cs) | _ -> () ) quals; 
          None 
        with Scanner cs -> Some cs 
      in
      let encoder = 
        match encoder with 
        | None -> ["b+=::flx::gc::generic::blit("^p^",sizeof("^tname^")); // prim"]
        | Some (CS.Str s) 
        | Some (CS.Str_template s) -> ["b+=::flx::gc::generic::string_blit("^ s ^ "("^p^")); //prim"]
        | Some _ -> assert false
      in
      encoder
     | _ -> 
      print_endline ("get_encoder encountered unexpected instance kind for type: " ^ sbt bsym_table t');
      print_endline ("Kind is " ^ string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) i );
      assert false
    end

  | BTYP_array (t,u) when unitsum bsym_table u = 0 -> []
  | BTYP_array (t,u) when unitsum bsym_table u > 0 -> 
    let k = unitsum bsym_table u in
    if k> 100 then
      failwith ("[get_encoder] Too many elements in array for shape, type " ^ sbt bsym_table t')
    else begin
      let eltype = cpp_typename syms bsym_table t in
      let seq = !(syms.counter) in incr (syms.counter);
      let index = "i"^si seq in 
      "//Array" ::
      ("for(::std::size_t "^index^"=0; "^index^"<"^si k^"; ++"^index^") {") ::
      get_encoder' syms bsym_table (p^"+"^index^"*sizeof("^eltype^")") t @
      ["}"]
    end

  | BTYP_tuple args ->
    let k = List.length args in
    "//Tuple"::
    List.concat ( List.map
    (fun (k,t) ->
      let s = "offsetof("^tname^",mem_"^si k^")" in
      (get_encoder' syms bsym_table (p^"+"^s) t)
    )
    (List.combine (nlist k) args))

  | BTYP_record (es) ->
    "//Record" ::
    List.concat (List.map 
    (fun (fld,t) ->
      let s = "offsetof("^tname^","^cid_of_flxid fld^")" in
      (get_encoder' syms bsym_table (p^"+"^s) t)
    )
    es
    )

  | _ -> assert false

(**********************************************************************)

let rec get_decoder' syms bsym_table p typ : string list =
  let tname = cpp_typename syms bsym_table typ in
  let t' = unfold "flx_cal_type_offsets: get_decoder" typ in
  if is_pod bsym_table typ
  then
    ["i=::flx::gc::generic::unblit("^p^",sizeof("^tname^"),s,i); // pod"]
  else match t' with
  | BTYP_vinst _ -> assert false
  | BTYP_inst (i,ts,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i
      with Not_found -> failwith
        ("get_decoder'] can't find index " ^ string_of_bid i)
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (vs,idts) ->
      ["i=::flx::gc::generic::blit("^p^",sizeof("^tname^"),s,i); // union"]

    | BBDCL_cstruct (vs, idts,_) (* this is NOT really correct ... *)
    | BBDCL_struct (vs,idts) ->
      let varmap = mk_varmap (Flx_bsym.sr bsym) vs ts in
      let idts = map (fun (s,t) -> s,varmap_subst varmap t) idts in
      let n = ref 0 in
      "//Struct" ::
      List.concat ( List.map 
      (fun (fld,t) ->
        let s= "offsetof("^tname^","^cid_of_flxid fld^")" in
        (get_decoder' syms bsym_table (p^"+"^s) t)
      )
      idts
      )

    | BBDCL_external_type (_,quals,_,_) ->
      let decoder = 
         try 
          List.iter (fun q-> match q with | `Decoder cs -> raise (Scanner cs) | _ -> () ) quals; 
          None 
        with Scanner cs -> Some cs 
      in
      let decoder = 
        match decoder with 
        | None -> ["i=::flx::gc::generic::unblit("^p^",sizeof("^tname^"),s,i); // prim"]
        | Some (CS.Str cs) 
        | Some (CS.Str_template cs) -> ["i="^ cs ^ "("^p^",s,i); //prim"]
        | Some _ -> assert false
      in
      decoder
     | _ -> assert false
    end

  | BTYP_array (t,u) when unitsum bsym_table u = 0 -> []
  | BTYP_array (t,u) when unitsum bsym_table u > 0 -> 
    let k = unitsum bsym_table u in
    if k> 100 then
      failwith ("[get_decoder] Too many elements in array for shape, type " ^ sbt bsym_table t')
    else begin
      let eltype = cpp_typename syms bsym_table t in
      let seq = !(syms.counter) in incr (syms.counter);
      let index = "i"^si seq in 
      "//Array" ::
      ("for(::std::size_t "^index^"=0; "^index^"<"^si k^"; ++"^index^") {") ::
      (get_decoder' syms bsym_table (p^"+"^index^"*sizeof("^eltype^")") t) @
      ["}"]
    end

  | BTYP_tuple args ->
    let k = List.length args in
    "//Tuple"::
    List.concat ( List.map
    (fun (k,t) ->
      let s = "offsetof("^tname^",mem_"^si k^")" in
      (get_decoder' syms bsym_table (p^"+"^s) t)
    )
    (List.combine (nlist k) args))

  | BTYP_record (es) ->
    "//Record" ::
    List.concat (List.map 
    (fun (fld,t) ->
      let s = "offsetof("^tname^","^cid_of_flxid fld^")" in
      (get_decoder' syms bsym_table (p^"+"^s) t)
    )
    es
    )

  | _ -> assert false


