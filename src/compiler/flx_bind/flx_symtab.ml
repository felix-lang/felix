let null_tab = Hashtbl.create 3

(* use fresh variables, but preserve names *)
let mkentry syms (vs:Flx_ast.ivs_list_t) i =
  let n = List.length (fst vs) in
  let base = !(syms.Flx_mtypes2.counter) in
  syms.Flx_mtypes2.counter := !(syms.Flx_mtypes2.counter) + n;
  let ts = List.map (fun i -> `BTYP_var (i+base,`BTYP_type 0)) (Flx_list.nlist n) in
  let vs = List.map2 (fun i (n,_,_) -> n,i+base) (Flx_list.nlist n) (fst vs) in
  (*
  print_endline ("Make entry " ^ string_of_int i ^ ", " ^ "vs =" ^
    Flx_util.catmap "," (fun (s,i) -> s ^ "<" ^ string_of_int i ^ ">") vs ^
    ", ts=" ^ Flx_util.catmap "," (Flx_print.sbt syms.Flx_mtypes2.dfns) ts
  );
  *)
  {Flx_types.base_sym=i; spec_vs=vs; sub_ts=ts}

let merge_ivs
  (vs1,{ Flx_ast.raw_type_constraint=con1; raw_typeclass_reqs=rtcr1 })
  (vs2,{ Flx_ast.raw_type_constraint=con2; raw_typeclass_reqs=rtcr2 }) :
  Flx_ast.ivs_list_t
  =
  let t =
    match con1,con2 with
    | `TYP_tuple[],`TYP_tuple[] -> `TYP_tuple[]
    | `TYP_tuple[],b -> b
    | a,`TYP_tuple[] -> a
    | `TYP_intersect a, `TYP_intersect b -> `TYP_intersect (a@b)
    | `TYP_intersect a, b -> `TYP_intersect (a @[b])
    | a,`TYP_intersect b -> `TYP_intersect (a::b)
    | a,b -> `TYP_intersect [a;b]
  and
    rtcr = Flx_list.uniq_list (rtcr1 @ rtcr2)
  in
  vs1 @ vs2,
  { Flx_ast.raw_type_constraint=t; raw_typeclass_reqs=rtcr }



let split_asms asms :
  (Flx_srcref.t * Flx_ast.id_t * int option * Flx_types.access_t * Flx_ast.vs_list_t * Flx_types.dcl_t) list *
  Flx_ast.sexe_t list *
  (Flx_srcref.t * Flx_types.iface_t) list *
  Flx_types.dir_t list
=
  let rec aux asms dcls exes ifaces dirs =
    match asms with
    | [] -> (dcls, exes, ifaces, dirs)
    | h :: t ->
      match h with
      | `Exe (sr,exe) -> aux t dcls ((sr,exe) :: exes) ifaces dirs
      | `Dcl (sr,id,seq,access,vs,dcl) -> aux t ((sr,id,seq,access,vs,dcl) :: dcls) exes ifaces dirs
      | `Iface (sr,iface) -> aux t dcls exes ((sr,iface) :: ifaces) dirs
      | `Dir dir -> aux t dcls exes ifaces (dir::dirs)
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

let full_add_unique syms sr (vs:Flx_ast.ivs_list_t) table key value =
  try
    let entry = Hashtbl.find table key in
    match entry with
    | `NonFunctionEntry (idx)
    | `FunctionEntry (idx :: _ ) ->
       (match Hashtbl.find syms.Flx_mtypes2.dfns (Flx_typing.sye idx)  with
       | { Flx_types.sr=sr2 } ->
         Flx_exceptions.clierr2 sr sr2
         ("[build_tables] Duplicate non-function " ^ key ^ "<" ^
         string_of_int (Flx_typing.sye idx) ^ ">")
       )
     | `FunctionEntry [] -> assert false
  with Not_found ->
    Hashtbl.add table key (`NonFunctionEntry (mkentry syms vs value))

let full_add_typevar syms sr table key value =
  try
    let entry = Hashtbl.find table key in
    match entry with
    | `NonFunctionEntry (idx)
    | `FunctionEntry (idx :: _ ) ->
       (match Hashtbl.find syms.Flx_mtypes2.dfns (Flx_typing.sye idx)  with
       | { Flx_types.sr=sr2 } ->
         Flx_exceptions.clierr2 sr sr2
         ("[build_tables] Duplicate non-function " ^ key ^ "<" ^
         string_of_int (Flx_typing.sye idx) ^ ">")
       )
     | `FunctionEntry [] -> assert false
  with Not_found ->
    Hashtbl.add table key
      (`NonFunctionEntry (mkentry syms Flx_ast.dfltvs value))


let full_add_function syms sr (vs:Flx_ast.ivs_list_t) table key value =
  try
    match Hashtbl.find table key with
    | `NonFunctionEntry entry ->
      begin
        match Hashtbl.find syms.Flx_mtypes2.dfns (Flx_typing.sye entry) with
        { Flx_types.id=id; sr=sr2 } ->
        Flx_exceptions.clierr2 sr sr2
        (
          "[build_tables] Cannot overload " ^
          key ^ "<" ^ string_of_int value ^ ">" ^
          " with non-function " ^
          id ^ "<" ^ string_of_int (Flx_typing.sye entry) ^ ">"
        )
      end

    | `FunctionEntry fs ->
      Hashtbl.remove table key;
      Hashtbl.add table key (`FunctionEntry (mkentry syms vs value :: fs))
  with Not_found ->
    Hashtbl.add table key (`FunctionEntry [mkentry syms vs value])


(* make_vs inserts unique indexes into vs_lists, thus creating an ivs_list. *)
let make_vs ?(print=false) level counter (vs', con) : Flx_ast.ivs_list_t =
  let vs =
    List.map begin fun (tid, tpat) ->
      let n = !counter in
      incr counter;
      if print then
        print_endline ("//  " ^ Flx_util.spaces level ^ string_of_int n ^
        " -> " ^ tid ^ " (type variable)");
      tid, n, tpat
    end vs'
  in
  vs, con


(* this routine takes a partially filled unbound definition table,
  'dfns' and a counter 'counter', and adds entries to the table
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


let rec build_tables syms name inherit_vs level parent grandparent root asms =
  (*
  print_endline ("//Building tables for " ^ name);
  *)

  (* Create the public and private symbol tables *)
  let pub_name_map = Hashtbl.create 97 in
  let priv_name_map = Hashtbl.create 97 in

  (* Split up the assemblies into their repsective types. split_asms returns
   * reversed lists, so we must undo that. *)
  let dcls,exes,ifaces,export_dirs = split_asms asms in
  let dcls,exes,ifaces,export_dirs =
    List.rev dcls, List.rev exes, List.rev ifaces, List.rev export_dirs
  in

  (* Add the parent to each unterface *)
  let ifaces = List.map (fun (i,j)-> i,j,parent) ifaces in
  let interfaces = ref ifaces in

  (* check root index. Error out if it's an invalid root. *)
  if level = 0 then begin
    if root <> !(syms.Flx_mtypes2.counter) then
      failwith "Wrong value for root index";

    begin match dcls with
    | [x] -> ()
    | _ -> failwith "Expected top level to contain exactly one module declaration"
    end;
    if name <> "root" then
      failwith ("Expected top level to be called root, got " ^ name)
  end else
    if name = "root" then
      failwith ("Can't name non-toplevel module 'root'")
    else
      Hashtbl.add priv_name_map "root"
        (`NonFunctionEntry (mkentry syms Flx_ast.dfltvs root));

  (* Step through each dcl and add the found assemblies to the symbol tables. *)
  List.iter (
    build_tables_from_dcl
      syms
      name
      inherit_vs
      level
      parent
      root
      pub_name_map
      priv_name_map
      ifaces
      interfaces
  ) dcls;

  pub_name_map,priv_name_map,exes,!interfaces, export_dirs


(** Add the symbols from one declaration. *)
and build_tables_from_dcl
  syms
  name
  inherit_vs
  level
  parent
  root
  pub_name_map
  priv_name_map
  ifaces
  interfaces
  (sr, id, seq, access, vs', dcl)
=
  let print_flag = syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag in

  (* Make some shorthand functions *)
  let dfns = syms.Flx_mtypes2.dfns in
  let counter = syms.Flx_mtypes2.counter in
  let spc = Flx_util.spaces level in
  let make_vs = make_vs ~print:print_flag level counter in

  if print_flag then
    print_endline (Flx_print.string_of_dcl level id seq vs' dcl);

  (* dummy-ish temporary symbol tables could contain type vars for looking
   * at this declaration. *)
  let pubtab = Hashtbl.create 3 in
  let privtab = Hashtbl.create 3 in

  (* Determine the next index to use. If we already have a symbol index,
   * use that, otherwise use the next number in the counter. *)
  let n = match seq with
  | Some n ->
      (* print_endline ("SPECIAL " ^ id ^ string_of_int n); *)
      n
  | None ->
      let n = !counter in incr counter;
      n
  in

  (* Update the type variable list to include the index. *)
  let vs = make_vs vs' in

  (*
  begin
    match vs with (_,{Flx_ast.raw_typeclass_reqs=rtcr})->
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
          match vs with (_,{Flx_ast.raw_typeclass_reqs=rtcr})-> addtc rtcr []
  in
  *)

  let add_unique table id idx = full_add_unique syms sr (merge_ivs vs inherit_vs) table id idx in
  let add_function table id idx = full_add_function syms sr (merge_ivs vs inherit_vs) table id idx in

  let add_tvars' parent table vs =
    List.iter begin fun (tvid,i,tpat) ->
      let mt = match tpat with
      | `AST_patany _ -> `TYP_type (* default/unspecified *)
      (*
      | #suffixed_name_t as name ->
          print_endline ("Decoding type variable " ^ string_of_int i ^ " kind");
          print_endline ("Hacking suffixed kind name " ^ string_of_suffixed_name name ^ " to TYPE");
          `TYP_type (* HACK *)
          *)

      | `TYP_none -> `TYP_type
      | `TYP_ellipsis -> Flx_exceptions.clierr sr "Ellipsis ... as metatype"
      | _ -> tpat
      in

      (* Add the symbol to the symbol table. *)
      Hashtbl.add dfns i {
        Flx_types.id=tvid;
        sr=sr;
        parent=parent;
        vs=Flx_ast.dfltvs;
        pubmap=null_tab;
        privmap=null_tab;
        dirs=[];
        symdef=`SYMDEF_typevar mt;
      };

      full_add_typevar syms sr table tvid i
    end
    (fst vs)
  in
  let add_tvars table = add_tvars' (Some n) table vs in

  (* Add parameters to the symbol table. *)
  let add_parameters parent fun_index ps =
    let ips = ref [] in

    List.iter begin fun (k,name,typ,dflt) ->
      let n = !counter in incr counter;

      if print_flag then
        print_endline ("//  " ^ spc ^ string_of_int n ^ " -> " ^ name ^ " (parameter)");

      (* Add the symbol to the symbol table. *)
      Hashtbl.add dfns n {
        Flx_types.id=name;
        sr=sr;
        parent=Some fun_index;
        vs=Flx_ast.dfltvs;
        pubmap=null_tab;
        privmap=null_tab;
        dirs=[];
        symdef=`SYMDEF_parameter (k,typ);
      };

      (* Register the symbol if it's public. *)
      if access = `Public then full_add_unique syms sr Flx_ast.dfltvs pubtab name n;

      (* Always register it in the private symbol table. *)
      full_add_unique syms sr Flx_ast.dfltvs privtab name n;

      ips := (k,name,typ,dflt) :: !ips
    end ps;
    List.rev !ips
  in

  (* Add simple parameters to the symbol table. *)
  let add_simple_parameters parent fun_index ps =
    let ips = ref [] in
    List.iter begin fun (name,typ) ->
      let n = !counter in incr counter;

      if print_flag then
        print_endline ("//  " ^ spc ^ string_of_int n ^ " -> " ^ name ^ " (parameter)");

      (* Add the symbol to the symbol table. *)
      Hashtbl.add dfns n {
        Flx_types.id=name;
        sr=sr;
        parent=Some fun_index;
        vs=Flx_ast.dfltvs;
        pubmap=null_tab;
        privmap=null_tab;
        dirs=[];
        symdef=`SYMDEF_parameter (`PVal,typ);
      };

      (* Register the symbol if it's public. *)
      if access = `Public then full_add_unique syms sr Flx_ast.dfltvs pubtab name n;

      (* Always register it in the private symbol table. *)
      full_add_unique syms sr Flx_ast.dfltvs privtab name n;

      ips := (`PVal,name,typ,None) :: !ips
    end ps;
    List.rev !ips
  in

  (* Add the declarations to the symbol table. *)
  match (dcl:Flx_types.dcl_t) with
  | `DCL_reduce (ps,e1,e2) ->
      let ips = add_simple_parameters parent n ps in

      (* Add the symbol to the symbol table. *)
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_reduce (ips, e1, e2);
      };

      add_tvars privtab

  | `DCL_axiom ((ps,pre),e1) ->
      let ips = add_parameters parent n ps in

      (* Add the symbol to the symbol table. *)
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_axiom ((ips, pre),e1);
      };

      add_tvars privtab

  | `DCL_lemma ((ps,pre),e1) ->
      let ips = add_parameters parent n ps in

      (* Add the symbol to the symbol table. *)
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_lemma ((ips, pre),e1);
      };

      add_tvars privtab


  (** XXX FIXME *)
  | `DCL_function ((ps,pre),t,props,asms) ->
      let is_ctor = List.mem `Ctor props in

      if is_ctor && id <> "__constructor__" then
        Flx_exceptions.syserr sr
          "Function with constructor property not named __constructor__";

      if is_ctor then
        begin match t with
        | `AST_void _ -> ()
        | _ -> Flx_exceptions.syserr sr "Constructor should return type void"
        end;

      (* change the name of a constructor to the class name prefixed by
       * _ctor_. *)
      let id = if is_ctor then "_ctor_" ^ name else id in

      (*
      if is_class && not is_ctor then
        print_endline ("TABLING METHOD " ^ id ^ " OF CLASS " ^ name);
      *)

      let fun_index = n in
      let t = if t = `TYP_none then `TYP_var fun_index else t in
      let pubtab,privtab, exes, ifaces,dirs =
        build_tables syms id Flx_ast.dfltvs (level+1)
        (Some fun_index) parent root asms
      in
      let ips = ref [] in
      List.iter begin fun (k,name,typ,dflt) ->
        let n = !counter in incr counter;

        if print_flag then print_endline
          ("//  " ^ spc ^ string_of_int n ^ " -> " ^ name ^ " (parameter)");

        Hashtbl.add dfns n {
          Flx_types.id=name;
          sr=sr;
          parent=Some fun_index;
          vs=Flx_ast.dfltvs;
          pubmap=null_tab;
          privmap=null_tab;
          dirs=[];
          symdef=`SYMDEF_parameter (k,typ);
        };
        if access = `Public then full_add_unique syms sr Flx_ast.dfltvs pubtab name n;
        full_add_unique syms sr Flx_ast.dfltvs privtab name n;
        ips := (k,name,typ,dflt) :: !ips
        end ps;

        Hashtbl.add dfns fun_index {
          Flx_types.id=id;
          sr=sr;
          parent=parent;
          vs=vs;
          pubmap=pubtab;
          privmap=privtab;
          dirs=dirs;
          symdef=`SYMDEF_function ((List.rev !ips,pre), t, props, exes);
        };

        if access = `Public then add_function pub_name_map id fun_index;
        add_function priv_name_map id fun_index;
        interfaces := !interfaces @ ifaces;
        add_tvars privtab

  | `DCL_match_check (pat,(mvname,match_var_index)) ->
      assert (List.length (fst vs) = 0);
      let fun_index = n in
      Hashtbl.add dfns fun_index {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_match_check (pat, (mvname,match_var_index));
      };
      if access = `Public then add_function pub_name_map id fun_index;
      add_function priv_name_map id fun_index;
      interfaces := !interfaces @ ifaces;
      add_tvars privtab

  | `DCL_match_handler (pat,(mvname,match_var_index),asms) ->
      (*
      print_endline ("Parent is " ^ match parent with Some i -> string_of_int i);
      print_endline ("Match handler, " ^ string_of_int n ^ ", mvname = " ^ mvname);
      *)
      assert (List.length (fst vs) = 0);
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
            (`AST_index (sr,mvname,match_var_index))
        in
        let dcl =
          `Dcl (sr, vname, None,`Private, Flx_ast.dfltvs, `DCL_val (`TYP_typeof (component)))
        and instr = `Exe (sr, `EXE_init (vname, component)) in
        new_asms := dcl :: instr :: !new_asms;
      end vars;

      (*
      print_endline ("asms are" ^ string_of_desugared !new_asms);
      *)
      let fun_index = n in
      let pubtab,privtab, exes,ifaces,dirs =
        build_tables syms id Flx_ast.dfltvs (level+1)
        (Some fun_index) parent root !new_asms
      in
      Hashtbl.add dfns fun_index {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=dirs;
        symdef=`SYMDEF_function (([],None),`TYP_var fun_index, [`Generated
        "symtab:match handler" ; `Inline],exes);
      };
      if access = `Public then
        add_function pub_name_map id fun_index;

      add_function priv_name_map id fun_index;
      interfaces := !interfaces @ ifaces;
      add_tvars privtab

  | `DCL_insert (s,ikind,reqs) ->
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_insert (s,ikind,reqs);
      };

      if access = `Public then add_function pub_name_map id n;
      add_function priv_name_map id n

  | `DCL_module asms ->
      let pubtab, privtab, exes, ifaces, dirs =
        build_tables
          syms
          id
          (merge_ivs inherit_vs vs)
          (level+1) (Some n)
          parent
          root
          asms
      in
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=dirs;
        symdef=`SYMDEF_module;
      };
      let n' = !counter in
      incr counter;
      let init_def = `SYMDEF_function ( ([],None),`AST_void sr, [],exes) in

      if print_flag then
        print_endline ("//  " ^ spc ^ string_of_int n' ^ " -> _init_  (module " ^ id ^ ")");

      Hashtbl.add dfns n' {
        Flx_types.id="_init_";
        sr=sr;
        parent=Some n;
        vs=vs;
        pubmap=null_tab;
        privmap=null_tab;
        dirs=[];
        symdef=init_def;
      };

      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;

      if access = `Public then add_function pubtab ("_init_") n';
      add_function privtab ("_init_") n';
      interfaces := !interfaces @ ifaces;
      add_tvars privtab

  | `DCL_typeclass asms ->
      (*
      let symdef = `SYMDEF_typeclass in
      let tvars = map (fun (s,_,_)-> `AST_name (sr,s,[])) (fst vs) in
      let stype = `AST_name(sr,id,tvars) in
      *)

      let pubtab, privtab, exes, ifaces, dirs =
        build_tables syms id (merge_ivs inherit_vs vs)
        (level+1) (Some n) parent root
        asms
      in
      let fudged_privtab = Hashtbl.create 97 in
      let vsl = List.length (fst inherit_vs) + List.length (fst vs) in
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
      let nts = List.map (fun (s,i,t)-> `BTYP_var (i,`BTYP_type 0)) (fst vs) in
      (* fudge the private view to remove the vs *)
      let show { Flx_types.base_sym=i; spec_vs=vs; sub_ts=ts } =
        string_of_int i ^ " |-> " ^
          "vs= " ^ Flx_util.catmap "," (fun (s,i) -> s ^ "<" ^ string_of_int i ^ ">") vs ^
          "ts =" ^ Flx_util.catmap  "," (Flx_print.sbt dfns) ts
      in
      let fixup ({ Flx_types.base_sym=i; spec_vs=vs; sub_ts=ts } as e) =
        let e' = {
          Flx_types.base_sym=i;
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
          | `NonFunctionEntry e -> `NonFunctionEntry (fixup e)
          | `FunctionEntry es -> `FunctionEntry (List.map fixup es)
        in
        Hashtbl.add fudged_privtab s nues
      end privtab;

      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=fudged_privtab;
        dirs=dirs;
        symdef=`SYMDEF_typeclass;
      };

      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;
      interfaces := !interfaces @ ifaces;
      add_tvars fudged_privtab

  | `DCL_instance (qn,asms) ->
      let pubtab,privtab, exes,ifaces,dirs =
        build_tables syms id Flx_ast.dfltvs
        (level+1) (Some n) parent root
        asms
      in
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=dirs;
        symdef=`SYMDEF_instance qn;
      };
      let inst_name = "_inst_" ^ id in
      if access = `Public then add_function pub_name_map inst_name n;
      add_function priv_name_map inst_name n;
      interfaces := !interfaces @ ifaces;
      add_tvars privtab

  | `DCL_val t ->
      let t = match t with | `TYP_none -> `TYP_var n | _ -> t in
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_val (t);
      };
      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;
      add_tvars privtab

  | `DCL_var t ->
      let t = if t = `TYP_none then `TYP_var n else t in
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=
          (* `SYMDEF_var (`TYP_lvalue t) *)
          `SYMDEF_var t;
      };
      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;
      add_tvars privtab

  | `DCL_lazy (t,e) ->
      let t = if t = `TYP_none then `TYP_var n else t in
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_lazy (t,e);
      };
      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;
      add_tvars privtab

  | `DCL_ref t ->
      let t = match t with | `TYP_none -> `TYP_var n | _ -> t in
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_ref (t);
      };
      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;
      add_tvars privtab

  | `DCL_type_alias (t) ->
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_type_alias t
      };

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
      | `TYP_typefun _ ->
          if access = `Public then add_function pub_name_map id n;
          add_function priv_name_map id n
      | _ ->
          if access = `Public then add_unique pub_name_map id n;
          add_unique priv_name_map id n
      end;
      add_tvars privtab

  | `DCL_inherit qn ->
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_inherit qn;
      };
      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n ;
      add_tvars privtab

  | `DCL_inherit_fun qn ->
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_inherit_fun qn;
      };
      if access = `Public then add_function pub_name_map id n;
      add_function priv_name_map id n;
      add_tvars privtab

  | `DCL_newtype t ->
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_newtype t;
      };
      let n_repr = !(syms.Flx_mtypes2.counter) in incr (syms.Flx_mtypes2.counter);
      let piname = `AST_name (sr,id,[]) in
      Hashtbl.add dfns n_repr {
        Flx_types.id="_repr_";
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_fun ([],[piname],t,`Identity,`NREQ_true,"expr");
      };
      add_function priv_name_map "_repr_" n_repr;
      let n_make = !(syms.Flx_mtypes2.counter) in incr (syms.Flx_mtypes2.counter);
      Hashtbl.add dfns n_make {
        Flx_types.id="_make_" ^ id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_fun ([],[t],piname,`Identity,`NREQ_true,"expr");
      };
      add_function priv_name_map ("_make_" ^ id) n_make;
      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;
      add_tvars privtab

  | `DCL_abs (quals,c, reqs) ->
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_abs (quals,c,reqs)
      };
      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;
      add_tvars privtab

  | `DCL_const (props,t,c, reqs) ->
      let t = if t = `TYP_none then `TYP_var n else t in
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_const (props,t,c,reqs);
      };
      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;
      add_tvars privtab

  | `DCL_fun (props, ts,t,c,reqs,prec) ->
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_fun (props, ts,t,c,reqs,prec)
      };
      if access = `Public then add_function pub_name_map id n;
      add_function priv_name_map id n;
      add_tvars privtab

  (* A callback is just like a C function binding .. only it actually generates
   * the function. It has a special argument the C function has as type void*,
   * but which Felix must consider as the type of a closure with the same type
   * as the C function, with this void* dropped. *)
  | `DCL_callback (props, ts,t,reqs) ->
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_callback (props, ts,t,reqs);
      };
      if access = `Public then add_function pub_name_map id n;
      add_function priv_name_map id n;
      add_tvars privtab

  | `DCL_union (its) ->
      let tvars = List.map (fun (s,_,_)-> `AST_name (sr,s,[])) (fst vs) in
      let utype = `AST_name(sr,id, tvars) in
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
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=`SYMDEF_union (its);
      };
      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;

      let unit_sum =
        List.fold_left begin fun v (_,_,_,t) ->
          v && (match t with `AST_void _ -> true | _ -> false)
        end true its
      in
      List.iter begin fun (component_name,ctor_idx,vs',t) ->
        let dfn_idx = !counter in incr counter; (* constructor *)
        let match_idx = !counter in incr counter; (* matcher *)

        (* existential type variables *)
        let evs = make_vs vs' in
        add_tvars' (Some dfn_idx) privtab evs;
        let ctor_dcl2 =
          if unit_sum then begin
            if access = `Public then add_unique pub_name_map component_name dfn_idx;
            add_unique priv_name_map component_name dfn_idx;
            `SYMDEF_const_ctor (n,utype,ctor_idx,evs)
          end else
            match t with
            | `AST_void _ -> (* constant constructor *)
                if access = `Public then add_unique pub_name_map component_name dfn_idx;
                add_unique priv_name_map component_name dfn_idx;
                `SYMDEF_const_ctor (n,utype,ctor_idx,evs)

            | `TYP_tuple ts -> (* non-constant constructor or 2 or more arguments *)
                if access = `Public then add_function pub_name_map component_name dfn_idx;
                add_function priv_name_map component_name dfn_idx;
                `SYMDEF_nonconst_ctor (n,utype,ctor_idx,evs,t)

            | _ -> (* non-constant constructor of 1 argument *)
                if access = `Public then add_function pub_name_map component_name dfn_idx;
                add_function priv_name_map component_name dfn_idx;
                `SYMDEF_nonconst_ctor (n,utype,ctor_idx,evs,t)
        in

        if print_flag then
          print_endline ("//  " ^ spc ^ string_of_int dfn_idx ^ " -> " ^ component_name);

          Hashtbl.add dfns dfn_idx {
            Flx_types.id=component_name;
            sr=sr;
            parent=parent;
            vs=vs;
            pubmap=pubtab;
            privmap=privtab;
            dirs=[];
            symdef=ctor_dcl2;
            };
      end its;
      add_tvars privtab

  | `DCL_cstruct (sts)
  | `DCL_struct (sts) ->
      (*
      print_endline ("Got a struct " ^ id);
      print_endline ("Members=" ^ Flx_util.catmap "; " (fun (id,t)->id ^ ":" ^ string_of_typecode t) sts);
      *)
      let tvars = List.map (fun (s,_,_)-> `AST_name (sr,s,[])) (fst vs) in
      let stype = `AST_name(sr,id,tvars) in
      Hashtbl.add dfns n {
        Flx_types.id=id;
        sr=sr;
        parent=parent;
        vs=vs;
        pubmap=pubtab;
        privmap=privtab;
        dirs=[];
        symdef=(
          match dcl with
          | `DCL_struct _ -> `SYMDEF_struct (sts)
          | `DCL_cstruct _ -> `SYMDEF_cstruct (sts)
          | _ -> assert false
        )
      };
      if access = `Public then add_unique pub_name_map id n;
      add_unique priv_name_map id n;
      add_tvars privtab

      (* NOTE: we don't add a type constructor for struct, because it would have
       * the same name as the struct type ..  we just check this case as
       * required *)


(* Public interface *)
let build_tables syms root asms =
  build_tables syms "root" Flx_ast.dfltvs 0 None None root asms
