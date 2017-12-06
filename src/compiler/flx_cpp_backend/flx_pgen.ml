open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bbdcl
open Flx_mtypes2
open Flx_typing
open Flx_name
open Flx_unify
open Flx_csubst
open List
open Flx_ctypes
open Flx_cexpr
open Flx_maps
open Flx_util
open Flx_print
open Flx_btype_subst

exception Found of Flx_btype.t

let rec shape_of' use_assoc_type syms bsym_table tn t =
  match t with
  | _ when islinear_type bsym_table t -> "::flx::rtl::cl_t_ptr_map"

  | BTYP_inst (i,ts,_) ->
    let bsym = Flx_bsym_table.find bsym_table i in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (vs,[id,n,[],t',_,false]) -> 
print_endline ("[flx_pgen] One component union should have been removed");
      let t'' = tsubst (Flx_bsym.sr bsym) vs ts t' in
      shape_of' use_assoc_type syms bsym_table tn t''

    | BBDCL_union (vs,idts) ->
      begin match Flx_vrep.cal_variant_rep bsym_table t with
      | Flx_vrep.VR_self -> assert false
      | Flx_vrep.VR_int -> "::flx::rtl::_int_ptr_map"
      | Flx_vrep.VR_nullptr -> "::flx::rtl::_address_ptr_map"
      | Flx_vrep.VR_packed -> "::flx::rtl::_address_ptr_map"
      | Flx_vrep.VR_uctor -> "::flx::rtl::_uctor_ptr_map"
      end
    (* special hack: if we have a type which has an associated gc_shape type,
     * use the shape of that instead of the original type. This is a hack because
     * it leaves no way to get the shape of the original type, however the 
     * needs_shape property is basically there for when that type isn't actually
     * allocated. The main example is when you have an immobile type, and so use
     * a pointer instead, but we still want to create an object of the original type
     * on the heap and return a pointer in a constructor.
     *
     * This would stuff up if we need to allocate the pointer, for example if it is
     * an argument to a union constuctor, however it is very unlikely csubst will
     * be used to generate such an expression (the shape use by the compiler will
     * be the pointer's shape in this case).
     *)
    | BBDCL_external_type (bvs,bquals,ct,breqs) ->
      if use_assoc_type then
        let get_assoc_type bqual = 
           match bqual with 
           | `Bound_needs_shape t -> raise (Found t)
           | _ -> ()
        in
        begin 
          try 
            List.iter get_assoc_type bquals;
            tn t ^ "_ptr_map"
          with Found t ->
            let t = tsubst (Flx_bsym.sr bsym) bvs ts t in
            tn t ^ "_ptr_map"
        end
      else
        tn t ^ "_ptr_map"

    | _ -> tn t ^ "_ptr_map"
    end

  | BTYP_rptsum _ 
  | BTYP_sum _ ->
      begin match Flx_vrep.cal_variant_rep bsym_table t with
      | Flx_vrep.VR_self -> assert false
      | Flx_vrep.VR_int -> "::flx::rtl::_int_ptr_map"
      | Flx_vrep.VR_nullptr -> "::flx::rtl::_address_ptr_map"
      | Flx_vrep.VR_packed -> "::flx::rtl::_address_ptr_map"
      | Flx_vrep.VR_uctor -> "::flx::rtl::_uctor_ptr_map"
      end
  | BTYP_variant _ -> "::flx::rtl::_uctor_ptr_map" 

  (* The shape of an actual function is CLASSNAME_ptr_map,
     The shape of a function **variable** is address_ptr_map, since it's
     just an ordinary pointer. In particular, you can't have the offsets
     into a function type, you need offsets into the actual function.
  *)
  | BTYP_function _ -> 
    (* print_endline "Function/procedure type shape: using address"; *)
    "::flx::rtl::_address_ptr_map"

  | BTYP_pointer _ -> "::flx::rtl::_address_ptr_map"
  | _ -> tn t ^ "_ptr_map"

let shape_of syms bsym_table shape_map tn t = 
  let sn = shape_of' true syms bsym_table tn t in
  Hashtbl.replace shape_map sn t;
  sn

let direct_shape_of syms bsym_table shape_map tn t = 
  let sn = shape_of' false syms bsym_table tn t in
  Hashtbl.replace shape_map sn t;
  sn

let gen_prim_call
  syms
  bsym_table
  (shapes: Flx_set.StringSet.t ref)
  (shape_map: (string, Flx_btype.t) Hashtbl.t)
  tsub
  ge
  ct
  ts
  (arg,argt as a)
  ret sr sr2 prec name
=
(*
  print_endline "Gen_prim_call data=";
  print_endline ("ct= "^ct);
  print_endline ("ts= "^catmap "," (sbt bsym_table) ts);
  print_endline ("argt = " ^ sbt bsym_table argt);
  print_endline ("arg = " ^ sbe bsym_table a);
*)
  (* we tolerate some errors at this point, in the hope the csubst won't 
   * actually use the bad types..
   *)
  let tn t = try cpp_typename syms bsym_table t with _ -> "TYPE_VARIABLE_NAME_ERROR" in
  let rt t = tsub t in
  let rtn t = tn (rt t) in

  let argt = rt argt in
  let tt = tn argt in
  let sh t = shape_of syms bsym_table shape_map tn t in
  let shret = sh ret in (* hmm .. argghhh .. *)
  let gshapes = map sh ts in
  let ts = map rtn ts in
  let carg () =
    match unfold "flx_pgen1" argt with
    | BTYP_tuple []  -> 
(* print_endline ("Flx_pgen: unit argument, ct = " ^ ct); *)
      ce_atom "0/*[gen_prim_call]CLT:UNIT*/"
    | x -> ge sr a 
(*
      try ge sr a 
      with exc -> 
       print_endline ("[flx_pgen] ERROR generating expression " ^ sbe bsym_table a);
       print_endline ("Diag: " ^ Printexc.to_string exc); 
       ce_atom "(ILLEGAL PASSING WHOLE TUPLE WITH CURRIED ARGUMENTS)"
*)
  in

  let ashape = sh argt in
  match arg,unfold "flx_pgen2" argt with

  (* the argument is explicitly a tuple *)
  | (BEXPR_tuple es,_) ->
    let ess =
      map
      (fun e->
        match e with
        (* individual arguments which are unit values are never passed:
          they CAN be passed as subcomponents though .. but they can't
          be generated .. we need to fix this!
        *)
        | BEXPR_tuple [],_ ->
          (*
          print_endline "Stripping unit";
          *)
          `Ce_atom "/*()*/"

        | _ -> 
          ge sr e
      )
      es
    in
    let ets,ashapes =
      match unfold "flx_pgen3" argt with
      | BTYP_tuple typs -> map rtn typs, map sh typs
      | BTYP_array (t,BTYP_unitsum n) ->
        let t = tn t
        and s = sh t
        in rev_map (fun _ -> t) (nlist n), rev_map (fun _ -> s) (nlist n)
      | _ -> assert false
    in
    csubst shapes sr sr2 ct 
      ~arg:carg 
      ~args:ess 
      ~typs:ets 
      ~argtyp:tt
      ~retyp:(tn ret)
      ~gargs:ts 
      ~prec:prec 
      ~argshape:ashape 
      ~argshapes:(shret::ashapes)
      ~display:["Error"] 
      ~gargshapes:gshapes
      ~name:name

  (* the argument isnt a tuple, but the type is *)
  | (_,BTYP_tuple typs) as x ->
    let typs = map rt typs in
    let k = List.length typs in
    let es = Flx_list.mapi
      (fun i t -> bexpr_get_n t i x)
      typs
    in
    let ess = map (ge sr) es in
    let ets = map tn typs in
    csubst shapes sr sr2 ct 
      ~arg:carg 
      ~args:ess 
      ~typs:ets 
      ~argtyp:tt 
      ~retyp:(tn ret) 
      ~gargs:ts 
      ~prec:prec 
      ~argshape:ashape 
      ~argshapes:(shret::(map sh typs))
      ~display:["Error"] 
      ~gargshapes:gshapes
      ~name:name

  (* the argument isnt a tuple, but the type is an array *)
  | (_,(BTYP_array(t,BTYP_unitsum n) as ta)) as x ->
    let t = rt t in
    let typs = map (fun _ -> rt t) (nlist n) in
    let es = Flx_list.range (fun i -> bexpr_get_n t i x) n in
    let ess = map (ge sr) es in
    let ets = map tn typs in
    csubst shapes sr sr2 ct 
      ~arg:carg ~args:ess 
      ~typs:ets ~argtyp:tt ~retyp:(tn ret) 
      ~gargs:ts 
      ~prec:prec 
      ~argshape:ashape 
      ~argshapes:(shret::map sh typs)
      ~display:["error"] 
      ~gargshapes:gshapes
      ~name:name

  (* the argument isn't an explicit tuple, and the type
     is neither an array nor tuple
  *)
  | (_,typ) ->
    csubst shapes sr sr2 ct 
    ~arg:carg ~args:[carg()] 
    ~typs:[tt] ~argtyp:tt ~retyp:(tn ret) 
    ~gargs:ts 
    ~prec:prec 
    ~argshape:ashape 
    ~argshapes:(shret::[ashape])
    ~display:["Error"] 
    ~gargshapes:gshapes
    ~name:name



