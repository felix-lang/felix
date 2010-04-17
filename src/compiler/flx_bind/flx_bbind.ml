open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bbdcl
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
  sym_table: Flx_sym_table.t;
  lookup_state: Flx_lookup.lookup_state_t;

  (* Used to cache which symbols we've already processed. *)
  visited: (Flx_types.bid_t, unit) Hashtbl.t;
}

(** The state needed for binding. *)
let make_bbind_state syms sym_table lookup_state =
  {
    syms = syms;
    sym_table = sym_table;
    lookup_state = lookup_state;
    visited = Hashtbl.create 97;
  }

let hfind msg h k =
  try Flx_sym_table.find h k
  with Not_found ->
    print_endline ("flx_bbind Flx_sym_table.find failed " ^ msg);
    raise Not_found

let find_param name_map s =
  match Hashtbl.find name_map s with
  | NonFunctionEntry (i) -> sye i
  | _ -> failwith ("[find_param] Can't find parameter " ^ s )

let print_bvs vs =
  if length vs = 0 then "" else
  "[" ^ catmap "," (fun (s,i) -> s ^ "<" ^ string_of_bid i ^ ">") vs ^ "]"

let rec find_true_parent sym_table child parent =
  match parent with
  | None -> None
  | Some parent ->
      match hfind "find_true_parent" sym_table parent with
      | _ -> Some parent

let bind_req state bsym_table env sr tag =
  (* HACKY *)
  try Some (Flx_lookup.lookup_code_in_env
    state.lookup_state
    bsym_table
    env
    sr
    tag)
  with Not_found -> None


(* this routine converts a requirements expression into a list
  of requirements. Note later if we have conflicts (negation),
  we'll need   to also return a list of requirements that
  would generate a conflict

  NOTE weird encoding: -1,[] is true (always satisfied)
  and -2,[] is false (impossible to satisfy)
*)

let bind_reqs bt state bsym_table env sr reqs =
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
    match bind_req state bsym_table env sr tag with
    | None -> [-2,[]]
    | Some (entries, ts) ->
      let ts = map bt ts in
      fold_left (fun lst index ->
        let index = sye index in
        if index = 0 then lst else
        try
          let ts = adjust_ts state.sym_table bsym_table sr index ts in
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

let rec bbind_symbol state bsym_table symbol_index sym =
  (* If we've already processed this bid, exit early. We do this so we can avoid
   * any infinite loops in the symbols. *)
  if Hashtbl.mem state.visited symbol_index then () else begin
  Hashtbl.add state.visited symbol_index ();

  let qname = qualified_name_of_index state.sym_table symbol_index in
  let true_parent = find_true_parent
    state.sym_table
    sym.Flx_sym.id
    sym.Flx_sym.parent
  in

  (* let env = Flx_lookup.build_env state.lookup_state state.sym_table parent in  *)
  let env = Flx_lookup.build_env
    state.lookup_state
    bsym_table
    (Some symbol_index)
  in
  (*
  print_endline "ENVIRONMENT:";
  print_env_short env;
  *)

  let bind_type_uses btype =
    (* Iterate through the now bound type and make sure to bind any referenced
     * bbdcls before continuing on. *)
    Flx_btype.iter ~f_bid:begin fun bid ->
      let sym = Flx_sym_table.find state.sym_table bid in
      bbind_symbol state bsym_table bid sym
    end btype
  in
  let bexes exes ret_type index tvars =
    let bexe_state = Flx_bind_bexe.make_bexe_state
      ?parent:sym.Flx_sym.parent
      ~env
      state.syms
      state.sym_table
      state.lookup_state
      tvars
      ret_type
    in
    let brt, bbexes = Flx_bind_bexe.bind_exes
      bexe_state
      bsym_table
      sym.Flx_sym.sr
      exes
    in
    bind_type_uses brt;
    brt, bbexes
  in
  (*
  print_endline ("Binding " ^ name ^ "<"^ si symbol_index ^ ">");
  print_endline ("Parent is " ^
    (match parent with | None -> "none" | Some i -> si i));
  print_endline ("True Parent is " ^
    (match true_parent with | None -> "none" | Some i -> si i));
  *)

  let be e =
    Flx_lookup.bind_expression
      state.lookup_state
      bsym_table
      env
      e
  in
  let luqn n =
    Flx_lookup.lookup_qn_in_env
      state.lookup_state
      bsym_table
      env
      n
  in
  let luqn2 n =
    Flx_lookup.lookup_qn_in_env2
      state.lookup_state
      bsym_table
      env
      n
  in
  let wrap_btype_uses f btype =
    let btype = f btype in

    bind_type_uses btype;

    (* Finally, return the type we bound previously. *)
    btype
  in
  let bt' t =
    (* Bind the type. *)
    Flx_lookup.bind_type
      state.lookup_state
      bsym_table
      env
      sym.Flx_sym.sr
      t
  in
  let bt = wrap_btype_uses bt' in
  let type_of_index = wrap_btype_uses (Flx_lookup.type_of_index
    state.lookup_state
    bsym_table)
  in

  (* this is the full vs list *)
  let ivs = find_vs state.sym_table bsym_table symbol_index in

  (* bind the type variables *)
  let bvs = map (fun (s,i,tp) -> s,i) (fst ivs) in

  let bind_type_constraint ivs =
    let cons =
      try
        Flx_tconstraint.build_type_constraints
          state.syms
          bt
          sym.Flx_sym.sr
          (fst ivs)
      with Not_found ->
        clierr sym.Flx_sym.sr "Can't build type constraints, type binding failed"
    in
    let {raw_type_constraint=icons} = snd ivs in
    let icons = bt icons in
    let cons = btyp_intersect [cons; icons] in
    cons
  in
  let bcons = bind_type_constraint ivs in
  let btraint = function | Some x -> Some (be x) | None -> None in
  let bind_reqs reqs =
    bind_reqs bt state bsym_table env sym.Flx_sym.sr reqs
  in
  let bind_quals quals = bind_quals bt quals in
  (*
  print_endline ("******Binding " ^ name);
  *)
  let bind_basic_ps ps =
    List.map (fun (k,s,t,_) ->
      let i = find_param sym.Flx_sym.privmap s in
      let t =
        let t = bt t in
        match k with
        | `PRef -> btyp_pointer t
        | _ -> t
      in
      { pid=s; pindex=i; pkind=k; ptyp=t }
    )
    ps
  in
  let bindps (ps,traint) =
    bind_basic_ps ps, btraint traint
  in
  let add_bsym parent bbdcl =
    let bsym = Flx_bsym.of_sym sym bbdcl in
    begin match parent with
    | None ->
        Flx_bsym_table.add_root bsym_table symbol_index bsym
    | Some parent ->
        let parent_sym = Flx_sym_table.find state.sym_table parent in
        bbind_symbol state bsym_table parent parent_sym;

        Flx_bsym_table.add_child bsym_table parent symbol_index bsym
    end
  in
  begin match sym.Flx_sym.symdef with
  (* Pure declarations of functions, modules, and type don't generate anything.
   * Variable dcls do, however. *)
  | SYMDEF_typevar _ -> ()

  | SYMDEF_module ->
    add_bsym true_parent (bbdcl_module ())

  | SYMDEF_reduce (ps,e1,e2) ->
    let bps = bind_basic_ps ps in
    let be1 = be e1 in
    let be2 = be e2 in
    state.syms.reductions <-
      (sym.Flx_sym.id,bvs,bps,be1,be2) :: state.syms.reductions;

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound reduction  " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^ print_bvs bvs);

    add_bsym true_parent (bbdcl_reduce ())

  | SYMDEF_axiom (ps,e1) ->
    let bps = bindps ps in
    let be1 = match e1 with
      | Predicate e -> `BPredicate (be e)
      | Equation (l,r) -> `BEquation (be l, be r)
    in
    state.syms.axioms <- (
      sym.Flx_sym.id,
      sym.Flx_sym.sr,
      sym.Flx_sym.parent,
      Axiom,
      bvs,
      bps,
      be1) :: state.syms.axioms;

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound axiom " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^ print_bvs bvs);

    add_bsym true_parent (bbdcl_axiom ())

  | SYMDEF_lemma (ps,e1) ->
    let bps = bindps ps in
    let be1 = match e1 with
      | Predicate e -> `BPredicate (be e)
      | Equation (l,r) -> `BEquation (be l, be r)
    in
    state.syms.axioms <- (
      sym.Flx_sym.id,
      sym.Flx_sym.sr,
      sym.Flx_sym.parent,
      Lemma,
      bvs,
      bps,
      be1) :: state.syms.axioms;

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound lemma " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^ print_bvs bvs);

    add_bsym true_parent (bbdcl_lemma ())

  | SYMDEF_function (ps,rt,props,exes) ->
    let bps = bindps ps in
    let ts = Flx_bparams.get_btypes bps in

    (* We don't need to bind the intermediary type. *)
    let brt = bt' rt in
    let brt, bbexes = bexes exes brt symbol_index bvs in
    let bbdcl = bbdcl_fun (props,bvs,bps,brt,bbexes) in

    (* Cache the type of the function. *)
    if not (Hashtbl.mem state.syms.ticache symbol_index) then begin
      let d = btyp_tuple ts in
      let ft =
        if mem `Cfun props
        then btyp_cfunction (d,brt)
        else btyp_function (d,brt)
      in
      let t = fold state.syms.counter ft in
      Hashtbl.add state.syms.ticache symbol_index t
    end;

    if state.syms.compiler_options.print_flag then begin
      let atyp = btyp_tuple ts in
      let t =
        if mem `Cfun props
        then btyp_cfunction (atyp,brt)
        else btyp_function (atyp,brt)
      in
      print_endline ("//bound function " ^ qname ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table t)
    end;

    add_bsym true_parent bbdcl

  | SYMDEF_parameter (k,_) ->
    begin match sym.Flx_sym.parent with
    | None -> failwith "[bbind_sym] expected parameter to have a parent"
    | Some ip ->
      match hfind "bbind" state.sym_table ip with
      | { Flx_sym.symdef=SYMDEF_reduce _}
      | { Flx_sym.symdef=SYMDEF_axiom _}
      | { Flx_sym.symdef=SYMDEF_lemma _}
      | { Flx_sym.symdef=SYMDEF_function _}
        ->
        let t = type_of_index symbol_index in
        let bbdcl = match k with
        | `PVal -> bbdcl_val (bvs,t,`Val)
        | `PVar -> bbdcl_val (bvs,t,`Var)
        | `PRef -> bbdcl_val (bvs,t,`Ref)
        | `PFun -> bbdcl_val (bvs, btyp_function (btyp_void (),t),`Val)
        in
        Hashtbl.add state.syms.varmap symbol_index t;

        if state.syms.compiler_options.print_flag then
          print_endline ("//bound val " ^ sym.Flx_sym.id ^ "<" ^
            string_of_bid symbol_index ^ ">" ^
            print_bvs bvs ^ ":" ^ sbt bsym_table t);

        add_bsym true_parent bbdcl

      | _ ->
        failwith ("[bbind_sym] expected parameter to have function or " ^
          "functor parent")
    end

  | SYMDEF_match_check (pat,(mvname,mvindex)) ->
    let t = type_of_index mvindex in
    let name_map = Hashtbl.create 97 in
    let exes =
      [
      sym.Flx_sym.sr, EXE_fun_return (
        gen_match_check pat (EXPR_index (sym.Flx_sym.sr,mvname,mvindex)))
      ]
    in
    let brt,bbexes = bexes exes flx_bbool symbol_index [] in

    if brt <> flx_bbool then
      failwith ("expected boolean return from match checker " ^ sym.Flx_sym.id ^
        " in\n" ^ Flx_srcref.short_string_of_src sym.Flx_sym.sr);

    (* Cache the type of the match. *)
    if not (Hashtbl.mem state.syms.ticache symbol_index) then begin
      let t = fold
        state.syms.counter
        (btyp_function (btyp_tuple [], flx_bbool))
      in
      Hashtbl.add state.syms.ticache symbol_index t
    end;

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound match check " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^
        ">" ^ print_bvs bvs ^ ":" ^ sbt bsym_table
        (btyp_function (btyp_tuple [], flx_bbool)));

    add_bsym true_parent (bbdcl_fun
      ([`Inline; `Generated "bbind: match check"], bvs, ([], None),
      flx_bbool, bbexes))

  | SYMDEF_const_ctor (uidx,ut,ctor_idx,vs') ->
    (*
    print_endline ("Binding const ctor " ^ sym.Flx_sym.id);
    *)
    let unit_sum =
      match hfind "bbind" state.sym_table uidx with
      | { Flx_sym.symdef=SYMDEF_union its} ->
        fold_left
        (fun v (_,_,_,t) ->
          v && (match t with TYP_void _ -> true | _ -> false)
        )
        true
        its
      | _ -> assert false
    in
    let t = type_of_index symbol_index in
    let ut = bt ut in
    let ct =
      if unit_sum then si ctor_idx
      else "_uctor_(" ^ si ctor_idx ^ ",0)"
    in

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound const " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">:" ^ sbt bsym_table t);

    add_bsym None (bbdcl_const ([], bvs, t, CS_str ct, []))

  | SYMDEF_nonconst_ctor (uidx,ut,ctor_idx,vs',argt) ->
    (*
    print_endline ("Binding non const ctor " ^ sym.Flx_sym.id);
    *)
    let t = type_of_index symbol_index in
    let argt = bt argt in
    let ut = bt ut in
    let btraint = bind_type_constraint vs' in
    let evs = map (fun (s,i,__) -> s,i) (fst vs') in

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound fun " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">:" ^ sbt bsym_table t);

    add_bsym None (bbdcl_nonconst_ctor (bvs,uidx,ut,ctor_idx,argt,evs,btraint))

  | SYMDEF_val t ->
    let t = type_of_index symbol_index in

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound val " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table t);

    add_bsym true_parent (bbdcl_val (bvs, t, `Val))

  | SYMDEF_var t ->
    let t = type_of_index symbol_index in

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound var " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table t);

    add_bsym true_parent (bbdcl_val (bvs, t, `Var))

  | SYMDEF_ref t ->
    let t = type_of_index symbol_index in

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound ref " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table t);

    add_bsym true_parent (bbdcl_val (bvs, t, `Ref))

  | SYMDEF_lazy (rt,e) ->
    let ps = [("dummy",`AST_void sym.Flx_sym.sr)],None in
    let exes = [sym.Flx_sym.sr, EXE_fun_return e] in

    (* We don't need to bind the intermediary type. *)
    let brt = bt' rt in
    let brt,bbexes = bexes exes brt symbol_index bvs in
    let props = [] in

    (* Cache the type of the lazy expression. *)
    if not (Hashtbl.mem state.syms.ticache symbol_index) then begin
      (* HACK! *)
      Hashtbl.add state.syms.ticache symbol_index brt
    end;

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound lazy " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table brt);

    add_bsym true_parent (bbdcl_fun (props,bvs,([],None),brt,bbexes))

  | SYMDEF_const (props,t,ct,reqs) ->
    let t = type_of_index symbol_index in
    let reqs = bind_reqs reqs in

    if state.syms.compiler_options.print_flag then
      print_endline ("//bound const " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table t);

    add_bsym true_parent (bbdcl_const (props,bvs,t,ct,reqs))

  | SYMDEF_fun (props,ts,ret,ct,reqs,prec) ->
    let ts = map bt ts in
    let bret = bt ret in
    let reqs = bind_reqs reqs in
    let bbdcl = bbdcl_external_fun (props,bvs,ts,bret,ct,reqs,prec) in

    (* Cache the type of the function. *)
    if not (Hashtbl.mem state.syms.ticache symbol_index) then begin
      let t = fold state.syms.counter (btyp_function (btyp_tuple ts, bret)) in
      Hashtbl.add state.syms.ticache symbol_index t
    end;

    if state.syms.compiler_options.print_flag then begin
      let atyp = btyp_tuple ts in
      print_endline ("//bound fun " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table (btyp_function (atyp, bret)))
    end;

    add_bsym true_parent bbdcl

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
        | TYP_name (_,id,[]) when id = sym.Flx_sym.id ->
          if !client_data_pos = -1 then
            client_data_pos := !counter
          ;
          let address = TYP_name (sym.Flx_sym.sr, "address", []) in
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
          | TYP_name (_,id,[]) when id = sym.Flx_sym.id -> false
          | t -> true
        )
        ts_orig
      )
    in
    let tf_args = match ts_f with
      | [x] -> x
      | lst -> btyp_tuple lst
    in
    let tf = btyp_function (tf_args, bret) in

    (* The type of the arguments Felix thinks the raw
       C function has on a call. A closure of this
       function is a Felix function .. NOT the raw
       C function.
    *)
    let ts_cf =
      map
      (function
        | TYP_name (_,id,[]) when id = sym.Flx_sym.id -> tf
        | t -> bt t
      )
      ts_orig
    in

    let prec = "postfix" in
    let reqs = bind_reqs reqs in

    (* Cache the type of the callback. *)
    if not (Hashtbl.mem state.syms.ticache symbol_index) then begin
      let t = fold state.syms.counter (btyp_cfunction (btyp_tuple ts_cf, bret)) in
      Hashtbl.add state.syms.ticache symbol_index t
    end;

    if state.syms.compiler_options.print_flag then begin
      let atyp = btyp_tuple ts_cf in
      print_endline ("//bound callback fun " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^ print_bvs bvs ^ ":" ^
        sbt bsym_table (btyp_function (atyp, bret)))
    end;

    add_bsym true_parent (bbdcl_callback
      (props,bvs,ts_cf,ts_c,!client_data_pos,bret,reqs,prec))

  | SYMDEF_union (cs) ->
    (*
    print_endline ("//Binding union " ^ si i ^ " --> " ^ name);
    *)
    let cs' = List.map (fun (n,v,vs',t) -> n, v,bt t) cs in
    add_bsym None (bbdcl_union (bvs, cs'))

  | SYMDEF_struct cs ->
    (* print_endline ("//Binding struct " ^ si i ^ " --> " ^ name);
    *)
    let cs' = List.map (fun (n,t) -> n, bt t) cs in
    add_bsym None (bbdcl_struct (bvs, cs'))

  | SYMDEF_cstruct cs ->
    (* print_endline ("//Binding struct " ^ si i ^ " --> " ^ name);
    *)
    let cs' = List.map (fun (n,t) -> n, bt t) cs in
    add_bsym None (bbdcl_cstruct (bvs, cs'))

  | SYMDEF_typeclass ->
    add_bsym true_parent (bbdcl_typeclass ([], bvs))

  | SYMDEF_instance qn ->
    (*
    print_endline "INSTANCE";
    *)
    let (k:entry_kind_t),(ts: typecode_t list) = luqn qn in
    let k = sye k in

    (* Make sure the typeclass is in the symbol table first. *)
    let typeclass_sym = Flx_sym_table.find state.sym_table k in
    bbind_symbol state bsym_table k typeclass_sym;

    (*
    print_endline ("binding ts = " ^ catmap "," string_of_typecode ts);
    *)
    let ts = map bt ts in
    (*
    print_endline "DOne ..";
    *)
    add_bsym true_parent (bbdcl_instance ([], bvs, bcons, k, ts))

  | SYMDEF_type_alias _ -> ()
  | SYMDEF_inherit _ -> ()
  | SYMDEF_inherit_fun _ -> ()

  | SYMDEF_abs (quals,ct,reqs)->
    (*
    print_endline ("//Binding abstract type " ^ si i ^ " --> " ^ name);
    *)
    let reqs = bind_reqs reqs in
    let bquals = bind_quals quals in
    add_bsym None (bbdcl_abs (bvs, bquals, ct, reqs))

  | SYMDEF_newtype t ->
    let t = bt t in
    add_bsym None (bbdcl_newtype (bvs, t))

  | SYMDEF_insert (ct,ikind,reqs) ->
    (* print_endline ("//Binding header string " ^ si i ^ " --> " ^ name);
    *)
    let reqs = bind_reqs reqs in
    add_bsym true_parent (bbdcl_insert (bvs, ct, ikind, reqs))
  end
  (*
  ;
  print_endline ("BINDING " ^ name ^ "<" ^ si i ^ "> COMPLETE");
  flush stdout
  *)
  end

let bbind state bsym_table =
  (* loop through all counter values [HACK]
    to get the indices in sequence, AND,
    to ensure any instantiations will be bound,
    (since they're always using the current value
    of state.counter for an index
  *)
  Flx_mtypes2.iter_bids begin fun i ->
    begin
      let entry =
        try Some (Flx_sym_table.find state.sym_table i)
        with Not_found -> None
      in match entry with
      | Some entry ->
        begin try
          (*
          begin
            try match hfind "bbind" state.sym_table !i with { Flx_sym.id=id} ->
              print_endline (" Trying to bind " ^ id ^ " index " ^ si !i)
            with Not_found ->
              failwith ("Binding error UNKNOWN SYMBOL, index " ^ si !i)
          end;
          *)
          bbind_symbol state bsym_table i entry
        with Not_found ->
          try match hfind "bbind" state.sym_table i with { Flx_sym.id=id } ->
            failwith ("Binding error, cannot find in table: " ^ id ^ " index " ^
              string_of_bid i)
          with Not_found ->
            failwith ("Binding error UNKNOWN SYMBOL, index " ^ string_of_bid i)
        end
      | None -> ()
    end
  end dummy_bid !(state.syms.counter)

let bind_interface (state:bbind_state_t) bsym_table = function
  | sr, IFACE_export_fun (sn, cpp_name), parent ->
      let env = Flx_lookup.build_env state.lookup_state bsym_table parent in
      let index,ts = Flx_lookup.lookup_sn_in_env
        state.lookup_state
        bsym_table
        env
        sn
      in
      if ts = [] then
        BIFACE_export_fun (sr,index, cpp_name)
      else clierr sr
      (
        "Can't export generic entity " ^
        string_of_suffixed_name sn
      )

  | sr, IFACE_export_python_fun (sn, cpp_name), parent ->
      let env = Flx_lookup.build_env state.lookup_state bsym_table parent in
      let index,ts =
        Flx_lookup.lookup_sn_in_env
        state.lookup_state
        bsym_table
        env
        sn
      in
      if ts = [] then
        BIFACE_export_python_fun (sr,index, cpp_name)
      else clierr sr
      (
        "Can't export generic entity " ^
        string_of_suffixed_name sn
      )

  | sr, IFACE_export_type (typ, cpp_name), parent ->
      let env = Flx_lookup.build_env state.lookup_state bsym_table parent in
      let t = Flx_lookup.bind_type
        state.lookup_state
        bsym_table
        env
        Flx_srcref.dummy_sr
        typ
      in
      if try var_occurs t with _ -> true then
      clierr sr
      (
        "Can't export generic- or meta- type " ^
        string_of_btypecode bsym_table t
      )
      else
        BIFACE_export_type (sr, t, cpp_name)
