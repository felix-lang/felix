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
open Flx_name_lookup

let debug = false 

(* calculate the transitive closure of an i,ts list
  with respect to inherit clauses.

  The result is an i,ts list.

  This is BUGGED because it ignores typeclass requirements ..
  however
  (a) modules can't have them (use inherit clause)
  (b) typeclasses don't use them (use inherit clause)
  (c) the routine is only called for modules and typeclasses?
*)

let rec get_includes lookup_qn_in_env' bind_type' state bsym_table rs xs =
  let rec get_includes' includes (((invs: ivs_list_t),i, ts) as index) =
    if not (List.mem index !includes) then
    begin
      (*
      if List.length ts != 0 then
        print_endline ("INCLUDES, ts="^catmap "," (sbt bsym_table) ts)
      ;
      *)
      includes := index :: !includes;
      let env = mk_bare_env state.sym_table i in (* should have ts in .. *)
      let qns,sr,vs =
        match hfind "lookup" state.sym_table i with
        { Flx_sym.id=id; sr=sr; vs=vs; dirs=dirs } ->
(*
print_endline "---------------------";
print_endline ("Includes, id = " ^ id);
*)
(*
        if List.length (fst invs) > 0 then begin
        print_endline (id ^", Ints=" ^ catmap "," (sbt bsym_table) ts);
        print_endline (id ^", Raw vs = " ^ catmap "," (fun (n,k,_) -> n ^ "<" ^ si k ^ ">") (fst vs));
        print_endline (id ^", In vs = " ^ catmap "," (fun (n,k,_) -> n ^ "<" ^ si k ^ ">") (fst invs));
        end;
*)
        let _,incl_qns,_ = split_dirs [] dirs in
        let vs = List.map (fun (n,i,mt) -> n,i,Flx_btype.bmt "Flx_env" mt) (fst vs) in
        incl_qns,sr,vs
      in
      List.iter (fun (invs2,qn) ->
          let rs2 = {rs with open_excludes = (invs,qn)::rs.open_excludes } in
          let {base_sym=j; spec_vs=vs'; sub_ts=ts'},ts'' =
            try lookup_qn_in_env' state bsym_table env rsground qn
            with Not_found -> failwith "QN NOT FOUND"
          in
            let mkenv i = mk_bare_env state.sym_table i in
            let args = List.map (fun (n,k,kind) -> 
(*
print_endline ("FUDGE: get includes: "^n^"=T<"^string_of_int k^">");
*)
              n,btyp_type_var (k,bmt "Flx_env" kind)) (fst invs2) in
            let bt t = bind_type' state bsym_table env rs sr t args mkenv in
            let ts'' = List.map bt ts'' in
            let ts'' = List.map (tsubst sr vs ts) ts'' in
            let ts' = List.map (tsubst sr vs' ts'') ts' in
let i1 : plain_ivs_list_t  = fst invs in
let i2 : plain_ivs_list_t = fst invs2 in
let i3 : plain_ivs_list_t  = i1 @ i2 in
let i4 = i3, snd invs in
            get_includes' includes (i4,j,ts')
      )
      qns
    end
  in
  let includes = ref [] in
  List.iter (get_includes' includes) xs;
  (* list is unique due to check during construction *)
  !includes

and bind_dir
  lookup_qn_in_env' bind_type'
  state
  bsym_table
  env rs
  (vs,qn)
=
(*
  print_endline ("Try to bind dir " ^ string_of_qualified_name qn);
*)
  let nullmap=Hashtbl.create 3 in
  (* cheating stuff to add the type variables to the environment *)
  let cheat_table = Hashtbl.create 7 in
  List.iter
  (fun (n,i,_) ->
   let entry = NonFunctionEntry {base_sym=i; spec_vs=[]; sub_ts=[]} in
    Hashtbl.add cheat_table n entry;
    if not (Flx_sym_table.mem state.sym_table i) then
      Flx_sym_table.add state.sym_table i None {
        Flx_sym.id=n;
        sr=dummy_sr;
        vs=dfltivs;
        pubmap=nullmap;
        privmap=nullmap;
        dirs=[];
        symdef=SYMDEF_typevar KND_type
      }
    ;
  )
  (fst vs)
  ;
  let cheat_env = (dummy_bid,"cheat",cheat_table,[],TYP_tuple []) in
  let rs2 = {rs with open_excludes = (vs,qn)::rs.open_excludes } in
  let {base_sym=i; spec_vs=spec_vs; sub_ts=ts}, ts' =
    try
      lookup_qn_in_env' state bsym_table env rs
      qn
    with Not_found -> failwith "QN NOT FOUND"
  in
  (* the vs is crap I think .. *)
  (*
  the ts' are part of the name and are bound in calling context
  the ts, if present, are part of a view we found if we
  happened to open a view, rather than a base module.
  At present this cannot happen because there is no way
  to actually name a view.
  *)
  (*
  assert (List.length vs = 0);
  assert (List.length ts = 0);
  *)
  let mkenv i = mk_bare_env state.sym_table i in
(*
  print_endline ("Binding ts=" ^ catmap "," string_of_typecode ts');
*)
  let ts' = List.map (fun t ->
    let btyp =  
      try (bind_type' state bsym_table (cheat_env::env) rs dummy_sr t [] mkenv) 
      with exn -> 
         print_endline ("[bind_dir] Type binding failed for " ^ string_of_typecode t); 
         raise exn
    in
    try
    beta_reduce "flx_lookup: bind_dir"
      state.counter
      bsym_table
      dummy_sr
      btyp
    with exn -> print_endline ("Beta-reduction failed, type " ^ sbt bsym_table btyp); raise exn
    ) ts' 
  in
  (*
  print_endline ("Ts bound = " ^ catmap "," (sbt bsym_table) ts');
  *)
  (*
  let ts' = List.map (fun t-> bind_type state env dummy_sr t) ts' in
  *)
  vs,i,ts'


and get_pub_tables lookup_qn_in_env' bind_type' state bsym_table env rs (dirs:sdir_t list) =
  let _,includes,_ = split_dirs rs.open_excludes dirs in
  let xs = uniq_list (List.map (bind_dir lookup_qn_in_env' bind_type' state bsym_table env rs) includes) in
  let includes = get_includes lookup_qn_in_env' bind_type' state bsym_table rs xs in
  let tables = List.map (Flx_pubname_map.pub_table_dir state.counter state.sym_table ) includes in
  tables

and merge_directives lookup_qn_in_env' bind_type' lookup_qn_in_env2' state bsym_table rs env name dirs typeclasses =
  let env = ref env in
  let add table =
   env :=
     match !env with
     | (idx, id, nm, nms,con) :: tail ->
     (idx, id, nm,  table :: nms,con) :: tail
     | [] -> assert false
  in
  let use_map = Hashtbl.create 97 in
  add use_map;

  let add_qn (vs, qn) =
    if List.mem (vs,qn) rs.open_excludes then () else
    begin
      let u = [bind_dir lookup_qn_in_env' bind_type' state bsym_table !env rs (vs,qn)] in
      let u = get_includes lookup_qn_in_env' bind_type' state bsym_table rs u in
      let tables = List.map (Flx_pubname_map.pub_table_dir  state.counter state.sym_table) u in
      List.iter add tables
    end
  in
  List.iter
  (fun (sr,dir) -> match dir with
  | DIR_inject_module (vs,qn) -> add_qn (vs,qn)
  | DIR_use (n,qn) ->
    begin let entry,_ = lookup_qn_in_env2' state bsym_table !env rs qn in
    match entry with

    | NonFunctionEntry _ ->
      if Hashtbl.mem use_map n
      then failwith "Duplicate non function used"
      else Hashtbl.add use_map n entry

    | FunctionEntry ls ->
      let entry2 =
        try Hashtbl.find use_map  n
        with Not_found -> FunctionEntry []
      in
      match entry2 with
      | NonFunctionEntry _ ->
        failwith "Use function and non-function kinds"
      | FunctionEntry ls2 ->
        Hashtbl.replace use_map n (FunctionEntry (ls @ ls2))
    end

  | DIR_open (vs,qn) -> add_qn (vs,qn)
  )
  dirs;

 (* these should probably be done first not last, because this is
 the stuff passed through the function interface .. the other
 opens are merely in the body .. but typeclasses can't contain
 modules or types at the moment .. only functions .. so it
 probably doesn't matter
 *)
(* AHEM, this has changed! Type classes can contain anything now! *)
 List.iter add_qn typeclasses;
 !env

(* UNUSED .. HMM .. 
and merge_opens state bsym_table env rs (typeclasses,opens,includes,uses) =
  let use_map = Hashtbl.create 97 in
  List.iter
  (fun (n,qn) ->
    let entry,_ = lookup_qn_in_env2' state bsym_table env rs qn in
    match entry with

    | NonFunctionEntry _ ->
      if Hashtbl.mem use_map n
      then failwith "Duplicate non function used"
      else Hashtbl.add use_map n entry

    | FunctionEntry ls ->
      let entry2 =
        try Hashtbl.find use_map  n
        with Not_found -> FunctionEntry []
      in
      match entry2 with
      | NonFunctionEntry _ ->
        failwith "Use function and non-function kinds"
      | FunctionEntry ls2 ->
        Hashtbl.replace use_map n (FunctionEntry (ls @ ls2))
  )
  uses
  ;

  (* convert qualified names to i,ts format *)
  let btypeclasses = List.map (bind_dir state bsym_table env rs) typeclasses in
  let bopens = List.map (bind_dir state bsym_table env rs) opens in

  (* HERE! *)

  let bincludes = List.map (bind_dir state bsym_table env rs) includes in

  (*
  (* HACK to check open typeclass *)
  let _ =
    let xs = get_includes state rs bopens in
    let tables = List.map (pub_table_dir state env true) xs in
    ()
  in
  *)
  (* strip duplicates *)
  let u = uniq_cat [] btypeclasses in
  let u = uniq_cat u bopens in
  let u = uniq_cat u bincludes in

  (* add on any inherited modules *)
  let u = get_includes state bsym_table rs u in

  (* convert the i,ts list to a list of lookup tables *)
  let tables = List.map (pub_table_dir state bsym_table env ) u in

  (* return the list with the explicitly renamed symbols prefixed
     so they can be used for clash resolution
  *)
  use_map::tables
*)

and build_env'' 
  lookup_qn_in_env' bind_type' lookup_qn_in_env2'
  state bsym_table rs index : env_t =
  let parent, sym = Flx_sym_table.find_with_parent state.sym_table index in
  let skip_merges = List.mem index rs.idx_fixlist in

  let rs = { rs with idx_fixlist = index :: rs.idx_fixlist } in
  let env = inner_build_env 
    lookup_qn_in_env' bind_type' lookup_qn_in_env2'
    state bsym_table rs parent 
  in
  
  (* Build temporary bare innermost environment with a full parent env. *)
  let typeclasses, constraints =
    let _, { raw_type_constraint=con; raw_typeclass_reqs=rtcr } =
      sym.Flx_sym.vs
    in
    rtcr,con
  in
  let env = (index, sym.Flx_sym.id, sym.Flx_sym.privmap, [], constraints) :: env in

  (* exit early if we don't need to do any merges *)
  if skip_merges then env else
  (*
  print_endline ("Build_env'' " ^ id ^":" ^ si index ^ " parent="^(match parent with None -> "None" | Some i -> si i));
  print_endline ("Privmap=");
  Hashtbl.iter (fun s _ ->  print_endline s) table ;
  *)

  (* use that env to process directives and type classes *)
  (*
  if typeclasses <> [] then
    print_endline ("Typeclass qns=" ^ catmap "," string_of_qualified_name typeclasses);
  *)
  let typeclasses = List.map (fun qn -> dfltivs,qn) typeclasses in

(*
  print_endline ("MERGE DIRECTIVES for " ^  sym.Flx_sym.id);
*)
  let env = merge_directives lookup_qn_in_env' bind_type' lookup_qn_in_env2'
    state bsym_table rs env sym.Flx_sym.id sym.Flx_sym.dirs typeclasses 
  in
(*
  print_endline "Build_env'' complete, DIRECTIVES MERGED";
*)
  env

and inner_build_env 
  lookup_qn_in_env' bind_type' lookup_qn_in_env2'
  state bsym_table rs parent : env_t =
  match parent with
  | None -> []
  | Some i ->
    try
      let env = Hashtbl.find state.env_cache i in
      env
    with
      Not_found ->
       let env = build_env'' 
       lookup_qn_in_env' bind_type' lookup_qn_in_env2'
       state bsym_table rs i in
       Hashtbl.add state.env_cache i env;
       env

and build_env 
  lookup_qn_in_env' bind_type' lookup_qn_in_env2'
  state bsym_table parent : env_t =
  (*
  print_endline ("Build env " ^ match parent with None -> "None" | Some i -> si i);
  *)
  inner_build_env 
  lookup_qn_in_env' bind_type' lookup_qn_in_env2'
  state bsym_table rsground parent


