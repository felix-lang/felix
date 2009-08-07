open Flx_util
open Flx_types
open Flx_ast
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_mbind
open Flx_unify
open Flx_exceptions
open List
open Flx_generic
open Flx_tpat

type bbind_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  bbdfns: Flx_types.fully_bound_symbol_table_t;
}

let make_bbind_state syms bbdfns = { syms=syms; bbdfns=bbdfns }

let hfind msg h k =
  try Hashtbl.find h k
  with Not_found ->
    print_endline ("flx_bbind Hashtbl.find failed " ^ msg);
    raise Not_found

let find_param name_map s =
  match Hashtbl.find name_map s with
  | NonFunctionEntry (i) -> sye i
  | _ -> failwith ("[find_param] Can't find parameter " ^ s )

let print_bvs vs =
  if length vs = 0 then "" else
  "[" ^ catmap "," (fun (s,i) -> s ^ "<"^si i^">") vs^ "]"

let rec find_true_parent dfns child parent =
  match parent with
  | None -> None
  | Some parent ->
    match hfind "find_true_parent" dfns parent with
    | {id=id; parent=grandparent; symdef=bdcl} ->
      match bdcl with
      | SYMDEF_module
        -> find_true_parent dfns id grandparent
      | _ -> Some parent

let bind_req syms env sr tag =
  (* HACKY *)
  try Some (Flx_lookup.lookup_code_in_env syms env sr tag)
  with _ -> None


(* this routine converts a requirements expression into a list
  of requirements. Note later if we have conflicts (negation),
  we'll need   to also return a list of requirements that
  would generate a conflict

  NOTE weird encoding: -1,[] is true (always satisfied)
  and -2,[] is false (impossible to satisfy)
*)

let bind_reqs bt syms env sr reqs : (bid_t * btypecode_t list) list =
  let add lst i =
    if
      lst = [-2,[]] or
      mem i lst or
      i = (0,[])
    then lst else i :: lst
  in
  let merge a b = fold_left add a b in
  let rec aux reqs = match reqs with
  | NREQ_true -> []
  | NREQ_false -> [-2,[]]
  | NREQ_and (a,b) -> merge (aux a) (aux b)
  | NREQ_or (a,b) ->
    let a = aux a and b = aux b in
    if a = [-2,[]] then b else a

  | NREQ_atom tag ->
    match bind_req syms env sr tag with
    | None -> [-2,[]]
    | Some (entries, ts) ->
      let ts = map bt ts in
      fold_left (fun lst index ->
        let index = sye index in
        if index = 0 then lst else
        try
          let ts = adjust_ts syms.dfns sr index ts in
          add lst (index,ts)
        with x ->
          print_endline "** Bind_req failed due to vs/ts mismatch";
          print_endline "** IGNORING! (HACK!!)";
          lst
      ) [] entries
  in
    let res = aux reqs in
    res

let bind_qual bt qual = match qual with
  | #base_type_qual_t as x -> x
  | `Raw_needs_shape t -> `Bound_needs_shape (bt t)

let bind_quals bt quals = map (bind_qual bt) quals

let bbind_symbol { syms=syms; bbdfns=bbdfns } symbol_index {
  id=name;
  sr=sr;
  parent=parent;
  vs=local_vs;
  privmap=name_map;
  dirs=dirs;
  symdef=bdcl
} =
  let qname = qualified_name_of_index syms.dfns symbol_index in
  let true_parent = find_true_parent syms.dfns name parent in
  let bexes env exes ret_type index tvars =
    let bexe_state = Flx_bexe.make_bexe_state
      syms
      env
      name
      index
      tvars
      ret_type
    in
    Flx_bexe.bind_exes bexe_state sr exes
  in
  (*
  print_endline ("Binding " ^ name ^ "<"^ si symbol_index ^ ">");
  print_endline ("Parent is " ^ (match parent with | None -> "none" | Some i -> si i));
  print_endline ("True Parent is " ^ (match true_parent with | None -> "none" | Some i -> si i));
  *)
  (* let env = Flx_lookup.build_env syms parent in  *)
  let env = Flx_lookup.build_env syms (Some symbol_index) in
  (*
  print_endline "ENVIRONMENT:";
  print_env_short env;
  *)

  let be e = Flx_lookup.bind_expression syms env e in
  let luqn n = Flx_lookup.lookup_qn_in_env syms env n in
  let luqn2 n = Flx_lookup.lookup_qn_in_env2 syms env n in
  let bt t = Flx_lookup.bind_type syms env sr t in

  (* bind the type variables *)
  let ivs = find_vs syms.dfns symbol_index in (* this is the full vs list *)
  let bvs = map (fun (s,i,tp) -> s,i) (fst ivs) in

  let bind_type_constraint ivs =
    let cons = try
      Flx_tconstraint.build_type_constraints syms bt sr (fst ivs)
      with _ -> clierr sr "Can't build type constraints, type binding failed"
    in
    let {raw_type_constraint=icons} = snd ivs in
    let icons = bt icons in
    let cons = BTYP_intersect [cons; icons] in
    cons
  in
  let bcons = bind_type_constraint ivs in
  let btraint = function | Some x -> Some (be x) | None -> None in
  let bind_reqs reqs = bind_reqs bt syms env sr reqs in
  let bind_quals quals = bind_quals bt quals in
  (*
  print_endline ("******Binding " ^ name);
  *)
  let bind_basic_ps ps =
    List.map (fun (k,s,t,_) ->
      let i = find_param name_map s in
      let t = let t = bt t in match k with `PRef -> BTYP_pointer t | _ -> t in
(*        print_endline ("Param " ^ s ^ " type=" ^ sbt syms.dfns t); *)
      {pid=s; pindex=i;pkind=k; ptyp=t}
    )
    ps
  in
  let bindps (ps,traint) =
    bind_basic_ps ps, btraint traint
  in
  let add_symbol symbol =
    Hashtbl.add bbdfns symbol_index symbol;
    Some symbol
  in
  begin match bdcl with
  (* Pure declarations of functions, modules, and type don't generate anything.
   * Variable dcls do, however. *)
  | SYMDEF_module
  | SYMDEF_typevar _
    -> None

  | SYMDEF_reduce (ps,e1,e2) ->
    let bps = bind_basic_ps ps in
    let be1 = be e1 in
    let be2 = be e2 in
    syms.reductions <- (name,bvs,bps,be1,be2) :: syms.reductions;

    if syms.compiler_options.print_flag then
      print_endline ("//bound reduction  " ^ name ^ "<" ^ si symbol_index ^
        ">" ^ print_bvs bvs);

    None

  | SYMDEF_axiom (ps,e1) ->
    let bps = bindps ps in
    let be1 = match e1 with
      | Predicate e -> `BPredicate (be e)
      | Equation (l,r) -> `BEquation (be l, be r)
    in
    syms.axioms <- (name, sr, parent, Axiom, bvs, bps, be1) :: syms.axioms;

    if syms.compiler_options.print_flag then
      print_endline ("//bound axiom " ^ name ^ "<" ^ si symbol_index ^ ">" ^
        print_bvs bvs);

    None

  | SYMDEF_lemma (ps,e1) ->
    let bps = bindps ps in
    let be1 = match e1 with
      | Predicate e -> `BPredicate (be e)
      | Equation (l,r) -> `BEquation (be l, be r)
    in
    syms.axioms <- (name, sr, parent, Lemma, bvs, bps, be1) :: syms.axioms;

    if syms.compiler_options.print_flag then
      print_endline ("//bound lemma " ^ name ^ "<" ^ si symbol_index ^ ">" ^
        print_bvs bvs);

    None

  | SYMDEF_function (ps,rt,props,exes) ->
    let bps = bindps ps in
    let ts = typeofbps_traint bps in
    let brt = bt rt in
    let brt', bbexes = bexes env exes brt symbol_index bvs in
    let bbdcl =
      match brt' with
      | BTYP_void -> BBDCL_procedure (props,bvs,bps,bbexes)
      | _ -> BBDCL_function (props,bvs,bps,brt',bbexes)
    in

    (* Cache the type of the function. *)
    if not (Hashtbl.mem syms.ticache symbol_index) then begin
      let d = typeoflist ts in
      let ft =
        if mem `Cfun props
        then BTYP_cfunction (d,brt')
        else BTYP_function (d,brt')
      in
      let t = fold syms.counter syms.dfns ft in
      Hashtbl.add syms.ticache symbol_index t
    end;

    if syms.compiler_options.print_flag then begin
      let atyp = typeoflist ts in
      let t =
        if mem `Cfun props
        then BTYP_cfunction (atyp,brt')
        else BTYP_function (atyp,brt')
      in
      print_endline ("//bound function " ^ qname ^ "<" ^ si symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt syms.dfns t)
    end;

    add_symbol (name, true_parent, sr, bbdcl)

  | SYMDEF_parameter (k,_) ->
    begin match parent with
    | None -> failwith "[bbind_sym] expected parameter to have a parent"
    | Some ip ->
      match hfind "bbind" syms.dfns ip with
      | {symdef=SYMDEF_reduce _}
      | {symdef=SYMDEF_axiom _}
      | {symdef=SYMDEF_lemma _}
      | {symdef=SYMDEF_function _}
        ->
        let t = Flx_lookup.type_of_index syms symbol_index in
        let dcl = match k with
        | `PVar -> BBDCL_var (bvs,t)
        | `PVal -> BBDCL_val (bvs,t)
        | `PRef -> BBDCL_val (bvs,t)
        | `PFun -> BBDCL_val (bvs,BTYP_function (BTYP_void,t))
        in
        Hashtbl.add syms.varmap symbol_index t;

        if syms.compiler_options.print_flag then
          print_endline ("//bound val " ^ name ^ "<" ^ si symbol_index ^ ">" ^
          print_bvs bvs ^ ":" ^ sbt syms.dfns t);

        add_symbol (name, true_parent, sr, dcl)

      | _ ->
        failwith ("[bbind_sym] expected parameter to have function or " ^
          "functor parent")
    end

  | SYMDEF_match_check (pat,(mvname,mvindex)) ->
    let t = Flx_lookup.type_of_index syms mvindex in
    let name_map = Hashtbl.create 97 in
    let exes =
      [
      sr,EXE_fun_return (gen_match_check pat (EXPR_index (sr,mvname,mvindex)))
      ]
    in
    let brt',bbexes = bexes env exes flx_bbool symbol_index [] in

    if brt' <> flx_bbool then
      failwith ("expected boolean return from match checker " ^ name ^
        " in\n" ^ Flx_srcref.short_string_of_src sr);

    (* Cache the type of the match. *)
    if not (Hashtbl.mem syms.ticache symbol_index) then begin
      let t = fold
        syms.counter
        syms.dfns
        (BTYP_function (BTYP_tuple [], flx_bbool))
      in
      Hashtbl.add syms.ticache symbol_index t
    end;

    if syms.compiler_options.print_flag then
      print_endline ("//bound match check " ^ name ^ "<" ^ si symbol_index ^
        ">" ^ print_bvs bvs ^ ":" ^ sbt syms.dfns
        (BTYP_function (BTYP_tuple[],flx_bbool)));

    add_symbol (name, true_parent, sr, BBDCL_function
        ([`Inline; `Generated "bbind: match check"], bvs, ([], None),
        flx_bbool, bbexes))

  | SYMDEF_const_ctor (uidx,ut,ctor_idx,vs') ->
    (*
    print_endline ("Binding const ctor " ^ name);
    *)
    let unit_sum =
      match hfind "bbind" syms.dfns uidx with
      | {symdef=SYMDEF_union its} ->
        fold_left
        (fun v (_,_,_,t) ->
          v && (match t with TYP_void _ -> true | _ -> false)
        )
        true
        its
      | _ -> assert false
    in
    let t = Flx_lookup.type_of_index syms symbol_index in
    let ut = bt ut in
    let ct =
      if unit_sum then si ctor_idx
      else "_uctor_(" ^ si ctor_idx ^ ",0)"
    in

    if syms.compiler_options.print_flag then
      print_endline ("//bound const " ^ name ^ "<" ^ si symbol_index ^ ">:" ^
        sbt syms.dfns t);

    add_symbol (name, None, sr, BBDCL_const ([], bvs, t, CS_str ct, []))

  | SYMDEF_nonconst_ctor (uidx,ut,ctor_idx,vs',argt) ->
    (*
    print_endline ("Binding non const ctor " ^ name);
    *)
    let t = Flx_lookup.type_of_index syms symbol_index in
    let argt = bt argt in
    let ut = bt ut in
    let btraint = bind_type_constraint vs' in
    let evs = map (fun (s,i,__) -> s,i) (fst vs') in
    let bbdcl = BBDCL_nonconst_ctor (bvs,uidx,ut,ctor_idx,argt,evs,btraint) in

    if syms.compiler_options.print_flag then
      print_endline ("//bound fun " ^ name ^ "<" ^ si symbol_index ^ ">:" ^
        sbt syms.dfns t);

    add_symbol (name, None, sr, bbdcl)

  | SYMDEF_val (t) ->
    let t = Flx_lookup.type_of_index syms symbol_index in

    if syms.compiler_options.print_flag then
      print_endline ("//bound val " ^ name ^ "<" ^ si symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt syms.dfns t);

    add_symbol (name, true_parent, sr, BBDCL_val (bvs, t))

  | SYMDEF_ref (t) ->
    let t = Flx_lookup.type_of_index syms symbol_index in

    if syms.compiler_options.print_flag then
      print_endline ("//bound ref " ^ name ^ "<" ^ si symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt syms.dfns t);

    add_symbol (name, true_parent, sr, BBDCL_ref (bvs, t))

  | SYMDEF_lazy (rt,e) ->
    let ps = [("dummy",`AST_void sr)],None in
    let exes = [sr,EXE_fun_return e] in
    let brt = bt rt in
    let brt',bbexes = bexes env exes brt symbol_index bvs in
    let props = [] in
    let bbdcl =
      BBDCL_function (props,bvs,([],None),brt',bbexes)
    in

    (* Cache the type of the lazy expression. *)
    if not (Hashtbl.mem syms.ticache symbol_index) then begin
      (* HACK! *)
      Hashtbl.add syms.ticache symbol_index brt'
    end;

    if syms.compiler_options.print_flag then
      print_endline ("//bound lazy " ^ name ^ "<" ^ si symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt syms.dfns brt');

    add_symbol (name, true_parent, sr, bbdcl)

  | SYMDEF_var (t) ->
    (*
    print_endline ("Binding variable " ^ name ^"<"^ si i ^">");
    *)
    let t = Flx_lookup.type_of_index syms symbol_index in

    if syms.compiler_options.print_flag then
      print_endline ("//bound var " ^ name ^ "<" ^ si symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt syms.dfns t);

    add_symbol (name, true_parent, sr, BBDCL_var (bvs, t))

  | SYMDEF_const (props,t,ct,reqs) ->
    let t = Flx_lookup.type_of_index syms symbol_index in
    let reqs = bind_reqs reqs in

    if syms.compiler_options.print_flag then
      print_endline ("//bound const " ^ name ^ "<" ^ si symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt syms.dfns t);

    add_symbol (name, true_parent, sr, BBDCL_const (props,bvs,t,ct,reqs))

  | SYMDEF_fun (props,ts,ret,ct,reqs,prec) ->
    let ts = map bt ts in
    let bret = bt ret in
    let reqs = bind_reqs reqs in
    let bbdcl = match bret with
      | BTYP_void -> BBDCL_proc (props,bvs,ts,ct,reqs)
      | _ -> BBDCL_fun (props,bvs,ts,bret,ct,reqs,prec)
    in

    (* Cache the type of the function. *)
    if not (Hashtbl.mem syms.ticache symbol_index) then begin
      let t = fold syms.counter syms.dfns (BTYP_function (typeoflist ts,bret)) in
      Hashtbl.add syms.ticache symbol_index t
    end;

    if syms.compiler_options.print_flag then begin
      let atyp = typeoflist ts in
      print_endline ("//bound fun " ^ name ^ "<" ^ si symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt syms.dfns (BTYP_function (atyp, bret)))
    end;

    add_symbol (name, true_parent, sr, bbdcl)

  | SYMDEF_callback (props,ts_orig,ret,reqs) ->
    let bret = bt ret in

    (* The type of the raw C function's arguments,
      using address = void* for the callback.
      This is the one passed to C, and the one we generate
      to cast the address to a Felix type and then execute it.

      Note the hack .. binding to C_hack::address .. it isn't
      necessary because we know it's a void*, but there is no
      builtin symbol for that.

      This is the function the user must call to actually
      invoke the Felix callback passed to it.

      A callback is much like an exported function,
      in that it binds a function to some arguments
      from a C call, however it is passed a closure,
      whereas exported functions create their own.

      This function isn't type safe to call at the C
      level, but it has the correct type to PASS to
      the usual establishing functions (or pointer to
      function in a struct)

      this is an extern "C" function with the original
      name. The name isn't mangled, and so shouldn't
      conflict with the typesafe ts_cf below.
    *)
    let client_data_pos = ref (-1) in
    let ts_c =
      let counter = ref 0 in
      map
      (function
        | TYP_name (_,id,[]) when id = name ->
          if !client_data_pos = -1 then
            client_data_pos := !counter
          ;
          let address = TYP_name(sr,"address",[]) in
          bt address
        | t -> incr counter; bt t
      )
      ts_orig
    in

    (* The type of the arguments of the Felix callback function,
      which are the same as the C function, but with the client
      data pointer dropped
    *)
    let ts_f =
      map bt
      (
        filter
        (function
          | TYP_name (_,id,[]) when id = name -> false
          | t -> true
        )
        ts_orig
      )
    in
    let tf_args = match ts_f with
      | [x] -> x
      | lst -> BTYP_tuple lst
    in
    let tf = BTYP_function (tf_args, bret) in

    (* The type of the arguments Felix thinks the raw
       C function has on a call. A closure of this
       function is a Felix function .. NOT the raw
       C function.
    *)
    let ts_cf =
      map
      (function
        | TYP_name (_,id,[]) when id = name -> tf
        | t -> bt t
      )
      ts_orig
    in

    let prec = "postfix" in
    let reqs = bind_reqs reqs in

    let bbdcl = BBDCL_callback (props,bvs,ts_cf,ts_c,!client_data_pos,bret,reqs,prec) in

    (* Cache the type of the callback. *)
    if not (Hashtbl.mem syms.ticache symbol_index) then begin
      let t = fold syms.counter syms.dfns (BTYP_cfunction (typeoflist ts_cf, bret)) in
      Hashtbl.add syms.ticache symbol_index t
    end;

    if syms.compiler_options.print_flag then begin
      let atyp = typeoflist ts_cf in
      print_endline ("//bound callback fun " ^ name ^ "<" ^ si symbol_index
        ^ ">" ^ print_bvs bvs ^ ":" ^
        sbt syms.dfns (BTYP_function (atyp, bret)))
    end;

    add_symbol (name, true_parent, sr, bbdcl)

  | SYMDEF_union (cs) ->
    (*
    print_endline ("//Binding union " ^ si i ^ " --> " ^ name);
    *)
    let cs' = List.map (fun (n,v,vs',t) -> n, v,bt t) cs in
    add_symbol (name, None, sr, BBDCL_union (bvs, cs'))

  | SYMDEF_struct (cs) ->
    (* print_endline ("//Binding struct " ^ si i ^ " --> " ^ name);
    *)
    let cs' = List.map (fun (n,t) -> n, bt t) cs in
    add_symbol (name, None, sr, BBDCL_struct (bvs, cs'))

  | SYMDEF_cstruct (cs) ->
    (* print_endline ("//Binding struct " ^ si i ^ " --> " ^ name);
    *)
    let cs' = List.map (fun (n,t) -> n, bt t) cs in
    add_symbol (name, None, sr, BBDCL_cstruct (bvs, cs'))

  | SYMDEF_typeclass ->
    add_symbol (name, true_parent, sr, BBDCL_typeclass ([], bvs))

  | SYMDEF_instance qn ->
    (*
    print_endline "INSTANCE";
    *)
    let (k:entry_kind_t),(ts: typecode_t list) = luqn qn in
    let k = sye k in
    (*
    print_endline ("binding ts = " ^ catmap "," string_of_typecode ts);
    *)
    let ts = map bt ts in
    (*
    print_endline "DOne ..";
    *)
    add_symbol (name, true_parent, sr, BBDCL_instance ([], bvs, bcons, k, ts))

  | SYMDEF_type_alias _ -> None
  | SYMDEF_inherit _ -> None
  | SYMDEF_inherit_fun _ -> None

  | SYMDEF_abs (quals,ct,reqs)->
    (*
    print_endline ("//Binding abstract type " ^ si i ^ " --> " ^ name);
    *)
    let reqs = bind_reqs reqs in
    let bquals = bind_quals quals in
    add_symbol (name, None, sr, BBDCL_abs (bvs, bquals, ct, reqs))

  | SYMDEF_newtype t ->
    let t = bt t in
    add_symbol (name, None, sr, BBDCL_newtype (bvs, t))

  | SYMDEF_insert (ct,ikind,reqs) ->
    (* print_endline ("//Binding header string " ^ si i ^ " --> " ^ name);
    *)
    let reqs = bind_reqs reqs in
    add_symbol (name, true_parent, sr, BBDCL_insert (bvs, ct, ikind, reqs))
  end
  (*
  ;
  print_endline ("BINDING " ^ name ^ "<" ^ si i ^ "> COMPLETE");
  flush stdout
  *)

let bbind bbind_state =
  (* loop through all counter values [HACK]
    to get the indices in sequence, AND,
    to ensure any instantiations will be bound,
    (since they're always using the current value
    of syms.counter for an index
  *)
  let i = ref 0 in
  while !i < !(bbind_state.syms.counter) do
    begin
      let entry =
        try Some (Hashtbl.find bbind_state.syms.dfns !i)
        with Not_found -> None
      in match entry with
      | Some entry ->
        begin try
          (*
          begin
            try match hfind "bbind" bbind_state.syms.dfns !i with {id=id} ->
              print_endline (" Trying to bind " ^ id ^ " index " ^ si !i)
            with Not_found ->
              failwith ("Binding error UNKNOWN SYMBOL, index " ^ si !i)
          end;
          *)
          ignore (bbind_symbol bbind_state !i entry)
        with Not_found ->
          try match hfind "bbind" bbind_state.syms.dfns !i with {id=id} ->
            failwith ("Binding error, cannot find in table: " ^ id ^ " index " ^ si !i)
          with Not_found ->
            failwith ("Binding error UNKNOWN SYMBOL, index " ^ si !i)
        end
      | None -> ()
    end
    ;
    incr i
  done

let bind_interface bbind_state = function
  | sr, IFACE_export_fun (sn, cpp_name), parent ->
      let env = Flx_lookup.build_env bbind_state.syms parent in
      let index,ts = Flx_lookup.lookup_sn_in_env bbind_state.syms env sn in
      if length ts = 0 then
        BIFACE_export_fun (sr,index, cpp_name)
      else clierr sr
      (
        "Can't export generic entity " ^
        string_of_suffixed_name sn
      )

  | sr, IFACE_export_python_fun (sn, cpp_name), parent ->
      let env = Flx_lookup.build_env bbind_state.syms parent in
      let index,ts = Flx_lookup.lookup_sn_in_env bbind_state.syms env sn in
      if length ts = 0 then
        BIFACE_export_python_fun (sr,index, cpp_name)
      else clierr sr
      (
        "Can't export generic entity " ^
        string_of_suffixed_name sn
      )

  | sr, IFACE_export_type (typ, cpp_name), parent ->
      let env = Flx_lookup.build_env bbind_state.syms parent in
      let t = Flx_lookup.bind_type bbind_state.syms env Flx_srcref.dummy_sr typ in
      if try var_occurs t with _ -> true then
      clierr sr
      (
        "Can't export generic- or meta- type " ^
        string_of_btypecode bbind_state.syms.dfns t
      )
      else
        BIFACE_export_type (sr, t, cpp_name)
