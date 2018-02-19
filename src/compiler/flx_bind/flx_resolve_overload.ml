open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_exceptions
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_typing2
open Flx_unify
open Flx_beta
open Flx_generic
open Flx_overload
open Flx_tpat
open Flx_lookup_state
open Flx_name_map
open Flx_btype_occurs
open Flx_btype_subst
open Flx_bid

let debugid = ""

let clone state bsym_table fi ft fbt fe fx (new_vs:plain_ivs_list_t) generic_alias index =
      begin 
      let parent,sym = Flx_sym_table.find_with_parent state.sym_table index in
      let {Flx_sym.id=id;sr=sr;vs=vs;pubmap=pubmap; privmap=privmap;dirs=dirs;symdef=symdef} = sym in
if id = debugid then
      print_endline ("CLONING SYMBOL " ^ id ^"<"^si index^">");

      let nupubmap =map_name_map fi fbt pubmap in 
      let nuprivmap = map_name_map fi fbt privmap in
      Hashtbl.iter (fun key value -> Hashtbl.replace nuprivmap key value)
      generic_alias;
(*
print_endline ("New public name map = " ^ string_of_name_map nupubmap);
print_endline ("New private name map = " ^ string_of_name_map nuprivmap);
*)
      let nudirs = dirs in
      let vs_list, {Flx_ast.raw_type_constraint=traint; raw_typeclass_reqs=tcreqs} = vs in

      let nusymdef = 
        match symdef with
        | SYMDEF_function (params,rett,effects,props,sexes) -> 
          let paramlist, ptraint = params in
          let rec map ps = match ps with
          | Satom  (sr,pkind,pname,ptyp,pinitopt) -> 
            Satom (sr,pkind,pname,ft ptyp, pinitopt (* HACK *))
          | Slist pss -> Slist (List.map map pss)
          in
          let nuparamlist = map paramlist in
          let nuptraint = match ptraint with | None -> None | Some e -> Some (fe e) in
          let nuparams = nuparamlist, nuptraint in
(*
      print_endline ("Remapped parameters: " ^ string_of_parameters nuparams);
*)
          let nurett = ft rett in
          let nueffects = ft effects in
          let nuprops = props in (* HACK, FIXME! *)
          let nusexes = List.map (fun (sr,x) -> sr,fx x) sexes in
(*
      print_endline ("New exes =\n");
      List.iter (fun (sr,x) -> print_endline (Flx_print.string_of_exe 2 x)) nusexes;
*)
          SYMDEF_function (nuparams, nurett, nueffects,nuprops, nusexes)
        | SYMDEF_insert (cs,ik,rqs) -> symdef (* HACK, FIXME! *)
        | SYMDEF_var t -> 
          let t = ft t in
(*
          print_endline ("Cloned VAR type = " ^ string_of_typecode t);
*)
          SYMDEF_var (t)
        | SYMDEF_val t -> 
          let t = ft t in
(*
          print_endline ("Cloned VAL type = " ^ string_of_typecode t);
*)
          SYMDEF_val (t)
        | SYMDEF_once t -> 
          let t = ft t in
(*
          print_endline ("Cloned ONCE type = " ^ string_of_typecode t);
*)
          SYMDEF_once (t)
        | SYMDEF_ref t -> 
          let t = ft t in
(*
          print_endline ("Cloned REF type = " ^ string_of_typecode t); 
*)
          SYMDEF_ref (t)
        | SYMDEF_parameter (k,t) -> 
          let t = ft t in
(*
          print_endline ("Cloned PARAMETER type = " ^ string_of_typecode t); 
*)
          SYMDEF_parameter (k,t)

        | SYMDEF_label label as x -> x

        | _ -> 
          print_endline ("[Flx_lookup:clone] Unhandled symdef");
          print_endline (string_of_symdef symdef id vs);
          print_endline ("Parent is " ^ match parent with | None -> "NONE" | Some i -> si i);
          assert false
      in
      let nuvs_vars = List.map (fun (s,i,mt) -> s,fi i, mt) new_vs in 
      let nutraint = ft traint in
      let nutcreqs = tcreqs in (* just a qualified name list *)
      let nuvs_aux = {Flx_ast.raw_type_constraint=nutraint; raw_typeclass_reqs=nutcreqs} in
      let nuvs = nuvs_vars,nuvs_aux in
      let nuparent = match parent with None -> None | Some i -> Some (fi i) in
      let nusym = {Flx_sym.id=id;sr=sr;vs=nuvs;pubmap=nupubmap; privmap=nuprivmap;dirs=nudirs;symdef=nusymdef} in
      let nuindex = fi index in
      Flx_sym_table.add state.sym_table nuindex nuparent nusym;
(*
print_endline ("++++ Cloned " ^ si index ^ " -> " ^ si nuindex);
*)
      end




let resolve_overload
  inner_build_env
  inner_bind_type
  inner_bind_expression
  lookup_qn_in_env2'
  trclose
  state
  bsym_table
  caller_env
  rs
  sr
  fs
  name
  sufs
  ts
=
if name = debugid then
print_endline ("Trying to resolve overload for " ^ name ^ ", lenfs=" ^ string_of_int (List.length fs));
  if List.length fs = 0 then None else
  let env i =
    (*
    print_endline ("resolve_overload: Building env for " ^ name ^ "<" ^ si i ^ ">");
    *)
    inner_build_env state bsym_table rs (Some i)
  in
  let bt rs sr i t =
    inner_bind_type state bsym_table (env i) sr rs t
  in
  let be i e =
    inner_bind_expression state bsym_table (env i) rs e
  in
  let luqn2 i qn = lookup_qn_in_env2' state bsym_table (env i) rs qn in
  let fs = trclose state bsym_table rs sr fs in
if name = debugid then
  print_endline ("Calling overload for " ^ name);
  let result : overload_result option =
    overload state.counter state.sym_table bsym_table caller_env rs bt be luqn2 sr fs name sufs ts
  in
  begin match result with
  | None -> 
if name = debugid then
    print_endline ("FAILED overload for " ^ name);
    None 
  | Some (index,sign,ret,mgu,ts) ->
if name = debugid then begin
    print_endline ("RESOLVED OVERLOAD OF " ^ name);
    print_endline (" .. mgu = " ^ string_of_varlist bsym_table mgu);
    print_endline ("Resolve ts = " ^ catmap "," (sbt bsym_table) ts);
end;
    let parent_vs,vs,{raw_typeclass_reqs=rtcr} = find_split_vs state.sym_table bsym_table index in
(*
if name = "accumulate" then begin
    print_endline ("Function vs=" ^ catmap "," svs vs);
    print_endline ("Parent vs=" ^ catmap "," svs parent_vs);
end;
*)
    let is_generic vs = List.fold_left (fun acc (s,i,mt) -> 
      acc || match mt with | KND_generic -> true | _ -> false) 
      false 
      vs 
    in
    assert (not (is_generic parent_vs));
    if not (is_generic vs) then result else let () = () in
if name = debugid then begin
    print_endline ("Found generic function " ^ name);
    print_endline ("REBINDING");
end;
    let new_vs = ref [] in
    let gen_vs = ref [] in
    let counter = ref 0 in
    let vmap = ref [] in
    let smap = ref [] in
    let parent_vs_len = List.length parent_vs in
    let parentts, funts = Flx_list.list_split ts parent_vs_len in
    let nufunts = ref [] in
    List.iter (fun (s,i,mt as v) -> 
      let n = !counter in
      begin match mt with 
      | KND_generic -> 
        let bt = List.nth ts n in
        let ubt = Flx_typecode_of_btype.typecode_of_btype bsym_table state.counter sr bt in
        gen_vs := (v,n) :: !gen_vs;
        smap := (s, ubt) :: !smap;
        vmap := (i, bt) :: !vmap
      | _ -> 
        new_vs := v :: !new_vs;
        nufunts := (List.nth funts n) ::!nufunts
      end;
      incr counter
      )
      vs
    ;
    let new_vs = List.rev (!new_vs) in
    let gen_vs = List.rev (!gen_vs) in

    (* smap lis list that says replace type named s with unbound type expression *) 
    let smap = List.rev (!smap) in

    (* vmap is list that says replace type variable index i with given bound type *)
    let vmap = List.rev (!vmap) in
    let nufunts = List.rev (!nufunts) in
(*
    print_endline ("Polymorphic vs = " ^ catmap "," svs new_vs);
    print_endline ("Generic vs = " ^ catmap "," (fun (v,n) -> "POS " ^ si n ^" " ^ 
      svs v^ " --> " ^ sbt bsym_table (List.nth ts n)
      ) gen_vs
    );
    List.iter (fun (ix,bt) -> print_endline ("Rebind " ^ si ix ^ " -> " ^ sbt bsym_table bt)) vmap;
    List.iter (fun (s,ubt) -> print_endline ("Rebind " ^ s ^ " -> " ^ string_of_typecode ubt)) smap;
*)
    let previous_rebinding =
       try Some (Hashtbl.find state.generic_cache (index, vmap))
       with Not_found -> None
    in
    match previous_rebinding with
    | Some result -> Some result (* weird but correct *)
    | None ->
    (* prevent the generator running twice *)
    (* we have to actually calculate a new rebinding right now, the reason is
      we need the return type!
    *)
(*
    print_endline "*** Calculating new rebinding";
*)
    let sym = Flx_sym_table.find state.sym_table index in
    let fresh () = fresh_bid state.counter in
    let remap_table = Hashtbl.create 97 in 
    let add i = let j = fresh() in (Hashtbl.add remap_table i j);j in
    let fi i = try Hashtbl.find remap_table i with Not_found -> i in
    let nuindex = add index in

    let descendants = Flx_sym_table.find_descendants state.sym_table index in
(*
    print_endline ("Descendants = " ^ catmap "," si descendants);
*)
    (* calculate rebinding map for descendants *)
    List.iter (fun i -> let _ = add i in ()) descendants;
(*
    Hashtbl.iter (fun i j -> print_endline ("  Rebind index " ^ si i ^ " -> " ^ si j)) remap_table;
*)
    let rec fbt t = match t with
      | Flx_btype.BTYP_type_var (i,mt) ->
(*
print_endline ("Examining bound type variable index " ^ si i);
*)
        begin
          try 
            let r = List.assoc i vmap in
(*
print_endline ("Replaced by vmap: " ^ sbt bsym_table r);
*)
            r
          with Not_found ->  
            let j = fi i in
(*
print_endline ("Not found in vmap, remaping with index remapper: " ^ si j);
*)
            Flx_btype.btyp_type_var (j, mt)
        end
      | x -> Flx_btype.map ~f_bid:fi ~f_btype:fbt x
    in
    let rec fe e = Flx_maps.full_map_expr fi ft fe e
    and ft t = match t with 
    | TYP_typeof e ->
      let e' = fe e in
(*
print_endline ("Unbound type remapper typeof " ^ string_of_expr e ^ " --> " ^ string_of_expr e');
*)
      TYP_typeof e'

    | TYP_var index -> 
      let j = fi index in
(*
print_endline ("Ubound type remapper type variable " ^ si index ^ " --> " ^ si j);
*)
      TYP_var j  
    | TYP_name (sr,tname,[]) as t -> 
(*
print_endline ("Trying to bind type name=" ^ tname);
*)
      begin
        try let r = List.assoc tname smap in
(*
           print_endline ("Rebound type name " ^ tname ^ " -> " ^ string_of_typecode r);
*)
           r 
        with Not_found -> t
      end
    | x -> Flx_maps.map_type ft x
    in 
    let fx x = Flx_maps.map_exe fi ft fe x in
 
    (* this code is ALMOST but not quite fully general.
       The problem is that the main function has its vs list and ts list
       reduced by the generic substitution, wherease the descendants do not
     *)
    assert (nuindex = fi index);
    let noalias = Hashtbl.create 97 in
    let generic_alias = Hashtbl.create 97 in
    List.iter (fun (s, t) ->
      let alias_index = fresh() in
(*
print_endline ("Adding alias " ^ s ^ "<"^si alias_index^"> -> " ^ string_of_typecode t);
*)
      let entry = NonFunctionEntry {base_sym=alias_index; spec_vs=[]; sub_ts=[]} in
      Hashtbl.add generic_alias s entry;
      let symdef = SYMDEF_type_alias t in
      let sym = {Flx_sym.id=s;sr=sr;vs=dfltivs;pubmap=noalias; privmap=noalias;dirs=[];symdef=symdef} in
      Flx_sym_table.add state.sym_table alias_index (Some nuindex) sym;
    )
    smap; 
    clone state bsym_table fi ft fbt fe fx new_vs generic_alias index;
(*
    print_endline ("Main symbol cloned and added");
*)
    (* HACK, FIXME! This will be the vs of the new symbol, for the master
       we have to replace the vs with one generics are stripped out of,
       but for kids, the vs is whatever they have. Use an option type
       instead to indicate an override or native calc.
    *)
    let hack_vs = [] in 
    List.iter (fun index -> 
      clone state bsym_table fi ft fbt fe fx hack_vs noalias index) 
    descendants;

(* end of cloning code, the rest is for recalculating the MGU *)
    let nuts = parentts @ nufunts in
(*
print_endline ("New  full ts = " ^ catmap "," (sbt bsym_table) nuts);
*)
    let numgu = mgu in (* HACK, FIXME! *)
(*
print_endline ("New mgu = " ^ catmap "," (fun (v,t) -> string_of_int v ^ "<-" ^ sbt bsym_table t) mgu);
*)
    let nusign = fbt sign in 
(*
print_endline ("New signature = " ^ sbt bsym_table nusign);
*)
    let nuret = fbt ret in (* HACK, FIXME! *)
(*
print_endline ("New return type = " ^ sbt bsym_table nuret);
*)
    let result_data = nuindex,nusign,nuret,numgu,nuts in
    Hashtbl.add state.generic_cache (index, vmap) result_data;
    Some result_data
  end


