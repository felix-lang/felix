open Flx_ast
open Flx_types
open Flx_btype

type t = {
  syms: Flx_mtypes2.sym_state_t;
  sym_table: Flx_sym_table.t;
  pub_name_map: (string, Flx_btype.entry_set_t) Hashtbl.t;
  priv_name_map: (string, Flx_btype.entry_set_t) Hashtbl.t;
}


(* use fresh variables, but preserve names *)
let mkentry syms (vs:ivs_list_t) i =
  let is = List.map
    (fun _ -> Flx_mtypes2.fresh_bid syms.Flx_mtypes2.counter)
    (fst vs)
  in
  let ts = List.map (fun i -> btyp_type_var (i, btyp_type 0)) is in
  let vs = List.map2 (fun i (n,_,_) -> n,i) is (fst vs) in
  (*
  print_endline ("Make entry " ^ string_of_bid ^ ", " ^ "vs =" ^
    Flx_util.catmap "," (fun (s,i) -> s ^ "<" ^ string_of_bid i ^ ">") vs ^
    ", ts=" ^ Flx_util.catmap "," (Flx_print.sbt sym_table) ts
  );
  *)
  { Flx_btype.base_sym=i; spec_vs=vs; sub_ts=ts }


let merge_ivs
  (vs1,{ raw_type_constraint=con1; raw_typeclass_reqs=rtcr1 })
  (vs2,{ raw_type_constraint=con2; raw_typeclass_reqs=rtcr2 }) :
  ivs_list_t
  =
  let t =
    match con1,con2 with
    | TYP_tuple[], TYP_tuple[] -> TYP_tuple[]
    | TYP_tuple[],b -> b
    | a, TYP_tuple[] -> a
    | TYP_intersect a, TYP_intersect b ->
        TYP_intersect (a@b)
    | TYP_intersect a, b -> TYP_intersect (a @[b])
    | a, TYP_intersect b -> TYP_intersect (a::b)
    | a,b -> TYP_intersect [a;b]
  and
    rtcr = Flx_list.uniq_list (rtcr1 @ rtcr2)
  in
  vs1 @ vs2,
  { raw_type_constraint=t; raw_typeclass_reqs=rtcr }


let split_asms asms =
  let rec aux asms dcls exes ifaces dirs =
    match asms with
    | [] -> (dcls, exes, ifaces, dirs)
    | h :: t ->
      match h with
      | Exe (sr,exe) -> aux t dcls ((sr,exe) :: exes) ifaces dirs
      | Dcl (sr,id,seq,access,vs,dcl) -> aux t ((sr,id,seq,access,vs,dcl) :: dcls) exes ifaces dirs
      | Iface (sr,iface) -> aux t dcls exes ((sr,iface) :: ifaces) dirs
      | Dir dir -> aux t dcls exes ifaces (dir::dirs)
  in
    aux asms [] [] [] []


let dump_name_to_int_map level name name_map =
  let spc = Flx_util.spaces level in
  print_endline (spc ^ "//Name to int map for " ^ name);
  print_endline (spc ^ "//---------------");
  Hashtbl.iter
  (
    fun id n ->
      print_endline ( "//" ^ spc ^ id ^ ": " ^ string_of_int n)
  )
  name_map
  ;
  print_endline ""


let strp = function | Some x -> string_of_int x | None -> "none"


let full_add_unique syms sym_table sr (vs:ivs_list_t) table key value =
  try
    let entry = Hashtbl.find table key in
    match entry with
    | NonFunctionEntry (idx)
    | FunctionEntry (idx :: _ ) ->
       (match Flx_sym_table.find sym_table (Flx_typing.sye idx) with
       | { Flx_sym.sr=sr2 } ->
         Flx_exceptions.clierr2 sr sr2
         ("[build_tables] Duplicate non-function " ^ key ^ "<" ^
         Flx_print.string_of_bid (Flx_typing.sye idx) ^ ">")
       )
     | FunctionEntry [] -> assert false
  with Not_found ->
    Hashtbl.add table key (NonFunctionEntry (mkentry syms vs value))


let full_add_typevar syms sym_table sr table key value =
  try
    let entry = Hashtbl.find table key in
    match entry with
    | NonFunctionEntry (idx)
    | FunctionEntry (idx :: _ ) ->
       (match Flx_sym_table.find sym_table (Flx_typing.sye idx)  with
       | { Flx_sym.sr=sr2 } ->
         Flx_exceptions.clierr2 sr sr2
         ("[build_tables] Duplicate non-function " ^ key ^ "<" ^
         Flx_print.string_of_bid (Flx_typing.sye idx) ^ ">")
       )
     | FunctionEntry [] -> assert false
  with Not_found ->
    Hashtbl.add table key
      (NonFunctionEntry (mkentry syms dfltvs value))


let full_add_function syms sym_table sr (vs:ivs_list_t) table key value =
  try
    match Hashtbl.find table key with
    | NonFunctionEntry entry ->
      begin
        match Flx_sym_table.find sym_table (Flx_typing.sye entry) with
        { Flx_sym.id=id; sr=sr2 } ->
        Flx_exceptions.clierr2 sr sr2
        (
          "[build_tables] Cannot overload " ^
          key ^ "<" ^ Flx_print.string_of_bid value ^ ">" ^
          " with non-function " ^
          id ^ "<" ^ Flx_print.string_of_bid (Flx_typing.sye entry) ^ ">"
        )
      end

    | FunctionEntry fs ->
      Hashtbl.remove table key;
      Hashtbl.add table key (FunctionEntry (mkentry syms vs value :: fs))
  with Not_found ->
    Hashtbl.add table key (FunctionEntry [mkentry syms vs value])


(* make_ivs inserts unique indexes into vs_lists, thus creating an ivs_list. *)
let make_ivs ?(print=false) level counter (vs, con) : ivs_list_t =
  let ivs =
    List.map begin fun (tid, tpat) ->
      let n = Flx_mtypes2.fresh_bid counter in
      if print then
        print_endline ("//  " ^ Flx_util.spaces level ^
          Flx_print.string_of_bid n ^ " -> " ^ tid ^ " (type variable)");
      tid, n, tpat
    end vs
  in
  ivs, con


(* this routine takes a partially filled unbound definition table,
  'sym_table' and a counter 'counter', and adds entries to the table
  at locations equal to and above the counter

  Each entity is also added to the name map of the parent entity.

  We use recursive descent, noting that the whilst an entity
  is not registered until its children are completely registered,
  its index is allocated before descending into child structures,
  so the index of children is always higher than its parent numerically

  The parent index is passed down so an uplink to the parent can
  be created in the child, but it cannot be followed until
  registration of all the children and their parent is complete
*)


let rec build_tables
  ~pub_name_map
  ~priv_name_map
  syms
  sym_table
  name
  inherit_ivs
  level
  parent
  root
  asms
=
  (*
  print_endline ("//Building tables for " ^ name);
  *)

  (* Split up the assemblies into their repsective types. split_asms returns
   * reversed lists, so we must undo that. *)
  let dcls, exes, ifaces, export_dirs = split_asms asms in
  let dcls, exes, ifaces, export_dirs =
    List.rev dcls, List.rev exes, List.rev ifaces, List.rev export_dirs
  in

  (* Add the parent to each interface *)
  let ifaces = List.map (fun (i,j)-> i, j, parent) ifaces in
  let interfaces = ref ifaces in

  (* check root index. Error out if it's an invalid root. *)
  if level = 0 then
    match dcls with
    | [x] -> ()
    | _ -> failwith "Expected top level to contain exactly one module declaration"
  else
    if name = "root" then
      failwith ("Can't name non-toplevel module 'root'")
    else
      Hashtbl.add priv_name_map "root"
        (NonFunctionEntry (mkentry syms dfltvs root));

  (* Step through each dcl and add the found assemblies to the symbol tables. *)
  List.iter (fun dcl ->
    ignore(build_table_for_dcl
      syms
      sym_table
      name
      inherit_ivs
      level
      parent
      root
      pub_name_map
      priv_name_map
      interfaces
      dcl)
  ) dcls;

  pub_name_map, priv_name_map, exes, !interfaces, export_dirs


(** Add the symbols from one declaration. *)
and build_table_for_dcl
  syms
  sym_table
  name
  inherit_ivs
  level
  parent
  root
  pub_name_map
  priv_name_map
  interfaces
  (sr, id, seq, access, vs, dcl)
=
  let print_flag = syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag in

  (* Make some shorthand functions *)
  let counter = syms.Flx_mtypes2.counter in
  let spc = Flx_util.spaces level in
  let make_ivs = make_ivs ~print:print_flag level counter in

  if print_flag then
    print_endline (Flx_print.string_of_dcl level id seq vs dcl);

  (* Determine the next index to use. If we already have a symbol index,
   * use that, otherwise use the next number in the counter. *)
  let symbol_index =
    match seq with
    | Some n ->
        (* print_endline ("SPECIAL " ^ id ^ string_of_int n); *)
        n
    | None ->
        Flx_mtypes2.fresh_bid counter
  in

  (* Update the type variable list to include the index. *)
  let ivs = make_ivs vs in

  (*
  begin
    match vs with (_,{raw_typeclass_reqs=rtcr})->
      match rtcr  with
      | _::_ ->
          print_endline (id^": TYPECLASS REQUIREMENTS " ^
          Flx_util.catmap "," string_of_qualified_name rtcr);
      | [] -> ();
  end;
  let rec addtc tcin dirsout = match tcin with
  | [] -> List.rev dirsout
  | h::t ->
      addtc t (DIR_typeclass_req h :: dirsout);
        in
        let typeclass_dirs =
          match vs with (_,{raw_typeclass_reqs=rtcr})-> addtc rtcr []
  in
  *)

  let add_unique table id idx = full_add_unique
    syms
    sym_table
    sr
    (merge_ivs ivs inherit_ivs)
    table
    id
    idx
  in
  let add_function table id idx = full_add_function
    syms
    sym_table
    sr
    (merge_ivs ivs inherit_ivs)
    table
    id
    idx
  in

  (* Add the symbol to the symbol table. *)
  let add_symbol
    ?(parent=parent)
    ?(ivs=ivs)
    ?pubtab
    ?privtab
    ?(dirs=[])
    index
    id
    symdef
  =
    let pubtab =
      match pubtab with
      | Some pubtab -> pubtab
      | None -> Hashtbl.create 0
    in
    let privtab =
      match privtab with
      | Some privtab -> privtab
      | None -> Hashtbl.create 0
    in
    Flx_sym_table.add sym_table index {
      Flx_sym.id = id;
      sr = sr;
      parent = parent;
      vs = ivs;
      pubmap = pubtab;
      privmap = privtab;
      dirs = dirs;
      symdef = symdef;
    }
  in

  let add_tvars' parent table ivs =
    List.iter begin fun (tvid, index, tpat) ->
      let mt = match tpat with
      | TYP_patany _ -> TYP_type (* default/unspecified *)
      (*
      | #suffixed_name_t as name ->
          print_endline ("Decoding type variable " ^ string_of_int i ^ " kind");
          print_endline ("Hacking suffixed kind name " ^ string_of_suffixed_name name ^ " to TYPE");
          TYP_type (* HACK *)
          *)

      | TYP_none -> TYP_type
      | TYP_ellipsis -> Flx_exceptions.clierr sr "Ellipsis ... as metatype"
      | _ -> tpat
      in

      (* Add the type variable to the symbol table. *)
      add_symbol ~ivs:dfltvs index tvid (SYMDEF_typevar mt);
      full_add_typevar syms sym_table sr table tvid index;
    end (fst ivs)
  in
  let add_tvars table = add_tvars' (Some symbol_index) table ivs in

  let add_parameter pubtab privtab parent (k, name, typ, dflt) =
    let n = Flx_mtypes2.fresh_bid counter in

    if print_flag then
      print_endline ("//  " ^ spc ^ Flx_print.string_of_bid n ^ " -> " ^
        name ^ " (parameter)");

    (* Add the paramater to the symbol table. *)
    add_symbol ~parent ~ivs:dfltvs n name (SYMDEF_parameter (k, typ));

    (* Possibly add the parameter to the public symbol table. *)
    if access = `Public then
      full_add_unique syms sym_table sr dfltvs pubtab name n;

    (* Add the parameter to the private symbol table. *)
    full_add_unique syms sym_table sr dfltvs privtab name n;

    (k, name, typ, dflt)
  in

  (* Add parameters to the symbol table. *)
  let add_parameters pubtab privtab parent =
    List.map (add_parameter pubtab privtab parent)
  in

  (* Add simple parameters to the symbol table. *)
  let add_simple_parameters pubtab privtab parent =
    List.map begin fun (name, typ) ->
      add_parameter pubtab privtab parent (`PVal, name, typ, None)
    end
  in

  (* dummy-ish temporary symbol tables could contain type vars for looking
   * at this declaration. *)
  let pubtab = Hashtbl.create 3 in
  let privtab = Hashtbl.create 3 in

  (* Add the declarations to the symbol table. *)
  begin match (dcl:Flx_types.dcl_t) with
  | DCL_reduce (ps, e1, e2) ->
      let ips = add_simple_parameters pubtab privtab (Some symbol_index) ps in

      (* Add the symbol to the symbol table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_reduce (ips, e1, e2));

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_axiom ((ps, pre), e1) ->
      let ips = add_parameters pubtab privtab (Some symbol_index) ps in

      (* Add the symbol to the symbol table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_axiom ((ips, pre),e1));

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_lemma ((ps, pre), e1) ->
      let ips = add_parameters pubtab privtab (Some symbol_index) ps in

      (* Add the symbol to the symbol table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_lemma ((ips, pre), e1));

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_function ((ps,pre),t,props,asms) ->
      let is_ctor = List.mem `Ctor props in

      if is_ctor && id <> "__constructor__" then
        Flx_exceptions.syserr sr
          "Function with constructor property not named __constructor__";

      if is_ctor then
        begin match t with
        | TYP_void _ -> ()
        | _ -> Flx_exceptions.syserr sr "Constructor should return type void"
        end;

      (* change the name of a constructor to the class name prefixed by
       * _ctor_. *)
      let id = if is_ctor then "_ctor_" ^ name else id in

      (*
      if is_class && not is_ctor then
        print_endline ("TABLING METHOD " ^ id ^ " OF CLASS " ^ name);
      *)
      let t = if t = TYP_none then TYP_var symbol_index else t in
      let pubtab, privtab, exes, ifaces, dirs =
        build_tables
          ~pub_name_map:(Hashtbl.create 97)
          ~priv_name_map:(Hashtbl.create 97)
          syms
          sym_table
          id
          dfltvs
          (level + 1)
          (Some symbol_index)
          root
          asms
      in

      let ips = add_parameters pubtab privtab (Some symbol_index) ps in

      (* Add the symbols to the sym_table. *)
      add_symbol ~pubtab ~privtab
        symbol_index id (SYMDEF_function ((ips, pre), t, props, exes));

      (* Possibly add the function to the public symbol table. *)
      if access = `Public then add_function pub_name_map id symbol_index;

      (* Add the function to the private symbol table. *)
      add_function priv_name_map id symbol_index;

      (* Add the interface. *)
      interfaces := !interfaces @ ifaces;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_match_check (pat,(mvname,match_var_index)) ->
      assert (List.length (fst ivs) = 0);

      (* Add the symbol to sym_table. *)
      add_symbol ~pubtab ~privtab symbol_index id
        (SYMDEF_match_check (pat, (mvname, match_var_index)));

      (* Possibly add the function to the public symbol table. *)
      if access = `Public then add_function pub_name_map id symbol_index;

      (* Add the function to the private symbol table. *)
      add_function priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_match_handler (pat,(mvname,match_var_index),asms) ->
      (*
      print_endline ("Parent is " ^ match parent with Some i -> string_of_int i);
      print_endline ("Match handler, " ^ string_of_int symbol_index ^ ", mvname = " ^ mvname);
      *)
      assert (List.length (fst ivs) = 0);
      let vars = Hashtbl.create 97 in
      Flx_mbind.get_pattern_vars vars pat [];
      (*
      print_endline ("PATTERN IS " ^ string_of_pattern pat ^ ", VARIABLE=" ^ mvname);
      print_endline "VARIABLES ARE";
      Hashtbl.iter begin fun vname (sr,extractor) ->
        let component =
          Flx_mbind.gen_extractor extractor (`AST_index (sr,mvname,match_var_index))
        in
        print_endline ("  " ^ vname ^ " := " ^ string_of_expr component);
      end vars;
      *)

      let new_asms = ref asms in
      Hashtbl.iter begin fun vname (sr,extractor) ->
        let component =
          Flx_mbind.gen_extractor
            extractor
            (EXPR_index (sr,mvname,match_var_index))
        in
        let dcl =
          Dcl (sr, vname, None,`Private, dfltvs,
            DCL_val (TYP_typeof (component)))
        and instr = Exe (sr, EXE_init (vname, component)) in
        new_asms := dcl :: instr :: !new_asms;
      end vars;

      (*
      print_endline ("asms are" ^ string_of_desugared !new_asms);
      *)
      let pubtab, privtab, exes, ifaces, dirs =
        build_tables
          ~pub_name_map:(Hashtbl.create 97)
          ~priv_name_map:(Hashtbl.create 97)
          syms
          sym_table
          id
          dfltvs
          (level + 1)
          (Some symbol_index)
          root
          !new_asms
      in

      (* Add symbols to sym_table. *)
      add_symbol ~pubtab ~privtab ~dirs symbol_index id (SYMDEF_function (
          ([],None),
          TYP_var symbol_index,
          [`Generated "symtab:match handler" ; `Inline],
          exes));

      (* Possibly add function to public symbol table. *)
      if access = `Public then add_function pub_name_map id symbol_index;

      (* Add function to private symbol table. *)
      add_function priv_name_map id symbol_index;

      (* Add interface *)
      interfaces := !interfaces @ ifaces;

      (* Add type variables to private symbol table. *)
      add_tvars privtab

  | DCL_insert (s,ikind,reqs) ->
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_insert (s,ikind,reqs));

      (* Possibly add the inserted function to the public symbol table. *)
      if access = `Public then add_function pub_name_map id symbol_index;

      (* Add the inserted function to the private symbol table. *)
      add_function priv_name_map id symbol_index

  | DCL_module asms ->
      let pubtab, privtab, exes, ifaces, dirs =
        build_tables
          ~pub_name_map:(Hashtbl.create 97)
          ~priv_name_map:(Hashtbl.create 97)
          syms
          sym_table
          id
          (merge_ivs inherit_ivs ivs)
          (level + 1)
          (Some symbol_index)
          root
          asms
      in
      (* Add the module to the sym_table. *)
      add_symbol ~pubtab ~privtab ~dirs symbol_index id SYMDEF_module;

      (* Take all the exes and add them to a function called _init_ that's
       * called when the module is loaded. *)
      let init_def = SYMDEF_function (([], None), TYP_void sr, [], exes) in

      (* Get a unique index for the _init_ function. *)
      let n' = Flx_mtypes2.fresh_bid counter in

      if print_flag then
        print_endline ("//  " ^ spc ^ Flx_print.string_of_bid n' ^
        " -> _init_  (module " ^ id ^ ")");

      (* Add the _init_ function to the sym_table. *)
      add_symbol ~parent:(Some symbol_index) n' "_init_" init_def;

      (* Possibly add module to the public symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the module to the private symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Possibly add the _init_ function to the public symbol table. *)
      if access = `Public then add_function pubtab "_init_" n';

      (* Add the _init_ function to the symbol table. *)
      add_function privtab "_init_" n';

      (* Add the interface. *)
      interfaces := !interfaces @ ifaces;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_typeclass asms ->
      (*
      let symdef = SYMDEF_typeclass in
      let tvars = map (fun (s,_,_)-> `AST_name (sr,s,[])) (fst ivs) in
      let stype = `AST_name(sr,id,tvars) in
      *)

      let pubtab, privtab, exes, ifaces, dirs =
        build_tables
          ~pub_name_map:(Hashtbl.create 97)
          ~priv_name_map:(Hashtbl.create 97)
          syms
          sym_table
          id
          (merge_ivs inherit_ivs ivs)
          (level + 1)
          (Some symbol_index)
          root
          asms
      in
      let fudged_privtab = Hashtbl.create 97 in
      let vsl = List.length (fst inherit_ivs) + List.length (fst ivs) in
      (*
      print_endline ("Strip " ^ string_of_int vsl ^ " vs");
      *)
      let drop vs =
        let keep = List.length vs - vsl in
        if keep >= 0 then
          List.rev (Flx_list.list_prefix (List.rev vs) keep)
        else
          failwith "WEIRD CASE"
      in
      let nts = List.map (fun (s,i,t)-> btyp_type_var (i,btyp_type 0)) (fst ivs) in
      (* fudge the private view to remove the vs *)
      let fixup ({ Flx_btype.base_sym=i; spec_vs=vs; sub_ts=ts } as e) =
        let e' = {
          Flx_btype.base_sym=i;
          spec_vs=drop vs;
          sub_ts=nts @ drop ts;
        } in

        (*
        print_endline (show e ^ " ===> " ^ show e');
        *)
        e'
      in

      Hashtbl.iter begin fun s es ->
        (*
        print_endline ("Entry " ^ s );
        *)
        let nues =
        if s = "root" then es else
          match es with
          | NonFunctionEntry e -> NonFunctionEntry (fixup e)
          | FunctionEntry es -> FunctionEntry (List.map fixup es)
        in
        Hashtbl.add fudged_privtab s nues
      end privtab;

      (* Add the typeclass to the sym_table. *)
      add_symbol
        ~pubtab
        ~privtab:fudged_privtab
        ~dirs
        symbol_index id SYMDEF_typeclass;

      (* Possibly add the typeclass to the public symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the typeclass to the private symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Add the interface. *)
      interfaces := !interfaces @ ifaces;

      (* Add the type variables to the private symbol table. *)
      add_tvars fudged_privtab

  | DCL_instance (qn,asms) ->
      let pubtab, privtab, exes, ifaces, dirs =
        build_tables
          ~pub_name_map:(Hashtbl.create 97)
          ~priv_name_map:(Hashtbl.create 97)
          syms
          sym_table
          id
          dfltvs
          (level + 1)
          (Some symbol_index)
          root
          asms
      in

      (* Add typeclass instance to the sym_table. *)
      add_symbol ~pubtab ~privtab ~dirs symbol_index id (SYMDEF_instance qn);

      (* Prepend _inst_ to the name of the instance.
       * XXX: Why do we need this? *)
      let inst_name = "_inst_" ^ id in

      (* Possibly add the typeclass instance to the public symbol table. *)
      if access = `Public then add_function pub_name_map inst_name symbol_index;

      (* Add the typeclass instance to the private symbol table. *)
      add_function priv_name_map inst_name symbol_index;

      (* Add the interface. *)
      interfaces := !interfaces @ ifaces;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_val t ->
      let t = match t with | TYP_none -> TYP_var symbol_index | _ -> t in

      (* Add the value to the dnfs. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_val t);

      (* Possibly add the value to the public symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the value to the private symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_var t ->
      let t = if t = TYP_none then TYP_var symbol_index else t in

      (* Add the variable to the sym_table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_var t);
      (*
      add_symbol symbol_index id (SYMDEF_var (TYP_lvalue t)
      *)

      (* Possibly add the variable to the public symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the variable to the private symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_lazy (t,e) ->
      let t = if t = TYP_none then TYP_var symbol_index else t in

      (* Add the lazy value to the sym_table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_lazy (t,e));

      (* Possibly add the lazy value to teh public symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the lazy value to the private symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_ref t ->
      let t = match t with | TYP_none -> TYP_var symbol_index | _ -> t in

      (* Add the reference value to the dnfs. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_ref t);

      (* Possibly add the reference value to the private symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the reference value to the public symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_type_alias t ->
      (* Add the type alias to the sym_table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_type_alias t);

      (* this is a hack, checking for a type function this way, since it will
       * also incorrectly recognize a type lambda like:
       *
       *    typedef f = fun(x:TYPE)=>x;
       *
       * With ordinary functions:
       *
       *    f := fun (x:int)=>x;
       *
       * initialises a value, and this f cannot be overloaded.
       *
       * That is, a closure (object) and a function (class) are distinguished ..
       * this should be the same for type functions as well.
       *
       * EVEN WORSE: our system is getting confused with unbound type variables
       * which are HOLES in types, and parameters, which are bound variables:
       * the latter are really just the same as type aliases where the alias
       * isn't known. The problem is that we usually substitute names with
       * what they alias, but we can't for parameters, so we replace them with
       * undistinguished type variables.
       *
       * Consequently, for a type function with a type function as a parameter,
       * the parameter name is being overloaded when it is applied, which is
       * wrong.
       *
       * We need to do what we do with ordinary function: put the parameter
       * names into the symbol table too: lookup_name_with_sig can handle this,
       * because it checks both function set results and non-function results.
       *)
      begin match t with
      | TYP_typefun _ ->
          if access = `Public then add_function pub_name_map id symbol_index;
          add_function priv_name_map id symbol_index
      | _ ->
          if access = `Public then add_unique pub_name_map id symbol_index;
          add_unique priv_name_map id symbol_index
      end;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_inherit qn ->
      (* Add the inherited typeclass to the dnfs. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_inherit qn);

      (* Possibly add the inherited typeclass to the public symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the inherited typeclass to the private symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_inherit_fun qn ->
      (* Add the inherited function to the dnfs. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_inherit_fun qn);

      (* Possibly add the inherited function to the public symbol table. *)
      if access = `Public then add_function pub_name_map id symbol_index;

      (* Add the inherited function to the private symbol table. *)
      add_function priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_newtype t ->
      (* Add the newtype to the sym_table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_newtype t);

      (* Create an identity function that doesn't do anything. *)
      let piname = TYP_name (sr,id,[]) in

      (* XXX: What's the _repr_ function for? *)
      let n_repr = Flx_mtypes2.fresh_bid syms.Flx_mtypes2.counter in

      (* Add the _repr_ function to the symbol table. *)
      add_symbol ~pubtab ~privtab n_repr "_repr_" (SYMDEF_fun (
        [],
        [piname],
        t,
        CS_identity,
        NREQ_true
        ,
        "expr"));

      (* Add the _repr_ function to the sym_table. *)
      add_function priv_name_map "_repr_" n_repr;

      (* XXX: What's the _make_ function for? *)
      let n_make = Flx_mtypes2.fresh_bid syms.Flx_mtypes2.counter in

      (* Add the _make_ function to the symbol table. *)
      add_symbol ~pubtab ~privtab n_make ("_make_" ^ id) (SYMDEF_fun (
        [],
        [t],
        piname,
        CS_identity,
        NREQ_true,
        "expr"));

      (* Add the _make_ function to the sym_table. *)
      add_function priv_name_map ("_make_" ^ id) n_make;

      (* Possibly add the _make_ function to the public symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the _make_ function to the private symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_abs (quals, c, reqs) ->
      (* Add the abs to the sym_table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_abs (quals, c, reqs));

      (* Possibly add the abs to the private symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the abs to the public symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_const (props, t, c, reqs) ->
      let t = if t = TYP_none then TYP_var symbol_index else t in

      (* Add the const to the sym_table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_const (props, t, c, reqs));

      (* Possibly add the const to the private symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the const public symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_fun (props, ts,t,c,reqs,prec) ->
      (* Add the function to the sym_table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_fun (props, ts, t, c, reqs, prec));

      (* Possibly add the function to the public symbol table. *)
      if access = `Public then add_function pub_name_map id symbol_index;

      (* Add the function to the private symbol table. *)
      add_function priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  (* A callback is just like a C function binding .. only it actually generates
   * the function. It has a special argument the C function has as type void*,
   * but which Felix must consider as the type of a closure with the same type
   * as the C function, with this void* dropped. *)
  | DCL_callback (props, ts,t,reqs) ->
      (* Add the callback to the sym_table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_callback (props, ts, t, reqs));

      (* Possibly add the callback to the public symbol table. *)
      if access = `Public then add_function pub_name_map id symbol_index;

      (* Add the callback to the private symbol table. *)
      add_function priv_name_map id symbol_index;

      (* Add the type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_union (its) ->
      let tvars = List.map (fun (s,_,_)-> TYP_name (sr,s,[])) (fst ivs) in
      let utype = TYP_name (sr, id, tvars) in
      let its =
        let ccount = ref 0 in (* count component constructors *)
        List.map begin fun (component_name,v,vs,t) ->
          (* ctor sequence in union *)
          let ctor_idx = match v with
          | None ->  !ccount
          | Some i -> ccount := i; i
          in
          incr ccount;
          component_name, ctor_idx, vs, t
        end its
      in

      (* Add union to sym_table. *)
      add_symbol ~pubtab ~privtab symbol_index id (SYMDEF_union its);

      (* Possibly add union to the public symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add the union to the private symbol table. *)
      add_unique priv_name_map id symbol_index;

      let unit_sum =
        List.fold_left begin fun v (_,_,_,t) ->
          v && (match t with TYP_void _ -> true | _ -> false)
        end true its
      in
      List.iter begin fun (component_name, ctor_idx, vs, t) ->
        let dfn_idx = Flx_mtypes2.fresh_bid counter in (* constructor *)
        let match_idx = Flx_mtypes2.fresh_bid counter in (* matcher *)

        (* existential type variables *)
        let evs = make_ivs vs in
        add_tvars' (Some dfn_idx) privtab evs;
        let ctor_dcl2 =
          if unit_sum then begin
            if access = `Public then add_unique pub_name_map component_name dfn_idx;
            add_unique priv_name_map component_name dfn_idx;
            SYMDEF_const_ctor (symbol_index, utype, ctor_idx, evs)
          end else
            match t with
            | TYP_void _ -> (* constant constructor *)
                if access = `Public then add_unique pub_name_map component_name dfn_idx;
                add_unique priv_name_map component_name dfn_idx;
                SYMDEF_const_ctor (symbol_index, utype, ctor_idx, evs)

            | TYP_tuple ts -> (* non-constant constructor or 2 or more arguments *)
                if access = `Public then add_function pub_name_map component_name dfn_idx;
                add_function priv_name_map component_name dfn_idx;
                SYMDEF_nonconst_ctor (symbol_index, utype, ctor_idx, evs, t)

            | _ -> (* non-constant constructor of 1 argument *)
                if access = `Public then add_function pub_name_map component_name dfn_idx;
                add_function priv_name_map component_name dfn_idx;
                SYMDEF_nonconst_ctor (symbol_index, utype, ctor_idx, evs, t)
        in

        if print_flag then
          print_endline ("//  " ^ spc ^ Flx_print.string_of_bid dfn_idx ^
            " -> " ^ component_name);

        (* Add the component to the sym_table. *)
        add_symbol ~pubtab ~privtab dfn_idx component_name ctor_dcl2;
      end its;

      (* Add type variables to the private symbol table. *)
      add_tvars privtab

  | DCL_cstruct (sts)
  | DCL_struct (sts) ->
      (*
      print_endline ("Got a struct " ^ id);
      print_endline ("Members=" ^ Flx_util.catmap "; " (fun (id,t)->id ^ ":" ^ string_of_typecode t) sts);
      *)
      let tvars = List.map (fun (s,_,_)-> `AST_name (sr,s,[])) (fst ivs) in
      let stype = `AST_name(sr, id, tvars) in

      (* Add symbols to sym_table *)
      add_symbol ~pubtab ~privtab symbol_index id (
        match dcl with
        | DCL_struct _ -> SYMDEF_struct (sts)
        | DCL_cstruct _ -> SYMDEF_cstruct (sts)
        | _ -> assert false
      );

      (* Possibly add the struct to the public symbol table. *)
      if access = `Public then add_unique pub_name_map id symbol_index;

      (* Add struct to the private symbol table. *)
      add_unique priv_name_map id symbol_index;

      (* Add type variables to the private symbol table. *)
      add_tvars privtab

      (* NOTE: we don't add a type constructor for struct, because it would have
       * the same name as the struct type ..  we just check this case as
       * required *)
  end;
  symbol_index


let make syms sym_table =
  {
    syms = syms;
    sym_table = sym_table;
    pub_name_map = Hashtbl.create 97;
    priv_name_map = Hashtbl.create 97;
  }


(* Add the interface to the symbol table. *)
let add_iface _ (sr, iface) =
  (* We don't currently use the symbol table. *)
  (sr, iface, None)


(* Add the declaration to symbol table. *)
let add_dcl ?parent state dcl =
  let level, pubmap, privmap =
    match parent with
    | Some index ->
        let symbol = Flx_sym_table.find
          state.sym_table
          index
        in
        1, symbol.Flx_sym.pubmap, symbol.Flx_sym.privmap
    | None ->
        0, state.pub_name_map, state.priv_name_map
  in

  let interfaces = ref [] in
  let symbol_index =
    build_table_for_dcl
      state.syms
      state.sym_table
      "root"
      Flx_ast.dfltvs
      level
      parent
      !(state.syms.Flx_mtypes2.counter)
      pubmap
      privmap
      interfaces
      dcl
  in
  symbol_index, !interfaces


(* Add the assemblies to the symbol table. *)
let add_asms state asms =
  let _, _, exes, interfaces, _ =
    build_tables
      ~pub_name_map:state.pub_name_map
      ~priv_name_map:state.priv_name_map
      state.syms
      state.sym_table
      "root"
      dfltvs
      0
      None
      !(state.syms.Flx_mtypes2.counter)
      asms
  in
  exes, interfaces
