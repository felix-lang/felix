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

module CS = Flx_code_spec
exception Scanner of CS.t

let unitsum bsym_table t = 
  try Flx_btype.int_of_linear_type bsym_table t 
  with Invalid_int_of_unitsum -> -1


let rec get_encoder' syms bsym_table p typ : string list =
  let tname = cpp_typename syms bsym_table typ in
  let t' = unfold "flx_cal_type_offsets: encoder" typ in
  if Flx_pod.is_pod bsym_table typ
  then
    ["b+=::flx::gc::generic::blit("^p^",sizeof("^tname^")); // pod"]
  else match t' with
  | BTYP_vinst _ -> assert false
  | BTYP_inst (`Nominal, i,ts,_) ->
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
  if Flx_pod.is_pod bsym_table typ
  then
    ["i=::flx::gc::generic::unblit("^p^",sizeof("^tname^"),s,i); // pod"]
  else match t' with
  | BTYP_vinst _ -> assert false
  | BTYP_inst (`Nominal, i,ts,_) ->
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


