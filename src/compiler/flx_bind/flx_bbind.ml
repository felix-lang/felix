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
open Flx_unify
open Flx_exceptions
open List
open Flx_generic
open Flx_tpat
open Flx_name_map
open Flx_bid
open Flx_bind_reqs
open Flx_bbind_state

let debug = false

let hfind msg h k =
  try Flx_sym_table.find h k
  with Not_found ->
    print_endline ("flx_bbind Flx_sym_table.find failed " ^ msg);
    raise Not_found

let find_param name_map s =
  match Hashtbl.find name_map s with
  | NonFunctionEntry (i) -> sye i
  | _ -> failwith ("[find_param] Can't find parameter " ^ s )

let print_bvs bvs =
  if length bvs = 0 then "" else
  "[" ^ catmap "," (fun (s,i,_) -> s ^ "<" ^ string_of_bid i ^ ">") bvs ^ "]"

(* confused over Some 0 and None parents .. I added this! *)
let rec find_true_parent sym_table child parent =
  match parent with
  | None -> None
  | Some 0 -> Some 0
  | Some parent ->
    let grandparent, sym = Flx_sym_table.find_with_parent sym_table parent in
    match sym.Flx_sym.symdef with
    | SYMDEF_library 
    | SYMDEF_module  -> find_true_parent sym_table child grandparent
    | SYMDEF_root _ -> None
    | _ -> Some parent



let bind_qual bt qual = match qual with
  | #base_type_qual_t as x -> x
  | `Raw_needs_shape t -> `Bound_needs_shape (bt t)
  | `Scanner cs -> `Scanner cs
  | `Finaliser cs -> `Finaliser cs
  | `Encoder cs -> `Encoder cs
  | `Decoder cs -> `Decoder cs

let bind_quals bt quals = map (bind_qual bt) quals

let str_parent p = match p with | Some p -> string_of_int p | None -> "None"

let rec bbind_symbol state bsym_table symbol_index sym_parent sym =
(*
  print_endline (" ^^^^ BBIND_SYMBOL (subroutine) : Binding symbol "^sym.Flx_sym.id^" index=" ^ string_of_int symbol_index);
*)
  (* If we've already processed this bid, exit early. We do this so we can avoid
   * any infinite loops in the symbols. *)
  if Hashtbl.mem state.visited symbol_index then 
  begin 
(* 
    print_endline ("Skipping already bound symbol " ^ string_of_int symbol_index); 
*)
    () 
  end 
  else 
  begin
  Hashtbl.add state.visited symbol_index ();

  (* even if not in visited, could be already there *)
  if Flx_bsym_table.mem bsym_table symbol_index then
  begin
(*
    print_endline ("Skipping already present symbol " ^ string_of_int symbol_index);
*)
    ()
  end
  else 
  (* warning .. naked "then" requires following "let", watch out for Ocaml's
   * screwed up syntax, don't put a print .. ; in here!
   *)
(*
  let _ = print_endline ("Processing bound symbol " ^ string_of_int symbol_index) in
*)
  let qname = qualified_name_of_index state.sym_table symbol_index in
  let true_parent = find_true_parent state.sym_table sym.Flx_sym.id sym_parent in
(*
print_endline ("Parent " ^ str_parent sym_parent ^ " mapped to true parent " ^ str_parent true_parent);
*)
  (* let env = Flx_lookup.build_env state.lookup_state state.sym_table parent in  *)
  let env = Flx_lookup.build_env
    state.lookup_state
    bsym_table
    (Some symbol_index)
  in
  
  (*
  print_endline "got ENVIRONMENT:";
  print_env_short env;
  *)

(* This is a bad idea on incremental build, because the "visited" list
   is local to this binding exercise and can't account for what's
   already in the table
*)
(*
  let bind_type_uses btype =
    (* Iterate through the now bound type and make sure to bind any referenced
     * bbdcls before continuing on. *)
    Flx_btype.iter ~f_bid:begin fun bid ->
      let parent, sym = Flx_sym_table.find_with_parent state.sym_table bid in
      if not (Flx_bsym_table.mem bsym_table bid) then begin
(*
print_endline (" &&&&&& bind_type_uses calling BBIND_SYMBOL");
*)
        bbind_symbol state bsym_table bid parent sym
      end
    end btype
  in
*)
  let bexes exes ret_type index tvars : Flx_btype.t * Flx_bexe.t list=
(*
  print_endline ("Flx_bbind.bexes: Binding " ^ sym.Flx_sym.id ^ "<"^ si symbol_index ^ ">");

  print_endline ("Flx_bbind.bexes: sym_parent is " ^
    (match sym_parent with | None -> "none" | Some i -> si i));
  print_endline ("Flx_bbind.bexes: True Parent is " ^
    (match true_parent with | None -> "none" | Some i -> si i));

*)
    let bexe_state = Flx_bexe_state.make_bexe_state
      ?parent:sym_parent
      ~env
      state.counter
      state.sym_table
      state.lookup_state
      tvars
      ret_type
    in
    let brt, bbexes = Flx_bind_bexe.bind_exes
      bexe_state
      bsym_table
      sym.Flx_sym.sr
      symbol_index
      sym.Flx_sym.id
      exes
    in
(*
    bind_type_uses brt;
*)
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
(*
  let wrap_btype_uses f btype =
    let btype = f btype in

    bind_type_uses btype;
    (* Finally, return the type we bound previously. *)
    btype
  in
*)
  let bt' t =
    (* Bind the type. *)
    Flx_lookup.bind_type
      state.lookup_state
      bsym_table
      env
      sym.Flx_sym.sr
      t
  in
(*
  let bt t = wrap_btype_uses bt' t in
  let type_of_index idx = wrap_btype_uses (Flx_lookup.type_of_index
    state.lookup_state
    bsym_table sym.Flx_sym.sr) idx
  in
*)
  let bt t = bt' t in
  let type_of_index idx = Flx_lookup.type_of_index
    state.lookup_state
    bsym_table sym.Flx_sym.sr idx
  in

  (* this is the full vs list *)
  let ivs = find_vs state.sym_table bsym_table symbol_index in
  let is_generic vs = List.fold_left (fun acc (name,index,typ) ->
      acc || match typ with | KND_generic -> true | _ -> false) 
      false
      vs
  in
  if is_generic (fst ivs) then begin
(*
     print_endline ("bind_symbol skipping symbol with generic type parameter: "^ 
       sym.Flx_sym.id ^"<"^ si symbol_index ^">")
*)
  end else
  (* bind the type variables *)
  let bvs = map (fun (s,i,tp) -> s,i, Flx_btype.bmt "Flx_bbind" tp) (fst ivs) in

  let bind_type_constraint ivs =
    let cons =
      try
        Flx_tconstraint.build_type_constraints
          state.counter
          bsym_table
          bt
          sym.Flx_sym.id
          sym.Flx_sym.sr
          (fst ivs)
      with Not_found ->
        clierrx "[flx_bind/flx_bbind.ml:332: E2] " sym.Flx_sym.sr "Can't build type constraints, type binding failed"
    in
    let {raw_type_constraint=icons} = snd ivs in
    let icons = bt icons in
    let cons = btyp_intersect [cons; icons] in
    cons
  in
  let bcons = bind_type_constraint ivs in
  let bcons = Flx_beta.beta_reduce "flx_bbind: constraint" state.counter bsym_table sym.Flx_sym.sr bcons in
  (*
  print_endline ("[flx_bbind] Constraint = " ^ sbt bsym_table bcons);
  *)
  let btraint = function | Some x -> Some (be x) | None -> None in
  let bind_reqs reqs =
    bind_reqs bt state bsym_table env sym.Flx_sym.sr reqs
  in
  let bind_quals quals = bind_quals bt quals in
  let rec bind_basic_ps ps = match ps with
     | Satom (sr,k,s,t,_) ->
      let i = find_param sym.Flx_sym.privmap s in
      let t =
        let t = bt t in
        match k with
        | _ -> t
      in
      Satom { pid=s; pindex=i; pkind=k; ptyp=t }
    | Slist pss -> Slist (List.map bind_basic_ps pss)
  in
  let bindps (ps,traint) =
    bind_basic_ps ps, btraint traint
  in
  let add_bsym parent bbdcl =
    let bsym = Flx_bsym.of_sym sym bbdcl in
    Flx_bsym_table.add bsym_table symbol_index parent bsym;
(*
    begin match parent with
    | None -> ()
    | Some parent ->
        let parent', sym' = Flx_sym_table.find_with_parent
          state.sym_table
          parent
        in
print_endline (" &&&&&& add_bsym calling BBIND_SYMBOL");
try
        bbind_symbol state bsym_table parent parent' sym';
with _ -> print_endline ("PARENT BINDING FAILED CONTINUING ANYHOW");
    end
*)
  in
(*
  print_endline ("******Binding " ^ qname ^ "="^ string_of_symdef
  sym.Flx_sym.symdef qname ivs);
*)
  begin match sym.Flx_sym.symdef with
  (* Pure declarations of functions, modules, and type don't generate anything.
   * Variable dcls do, however. *)
  (* CHANGED! Modules and root DO generate stuff now: their initialisation code.
   * Primitive functions * definitions don't generate anything.. there are no
   * pure declarations of functions.
   *)

  | SYMDEF_typevar _ -> ()

  (* the root module doesn't generate anything YET. After the complete 
     program has been bound, THEN and only then we can extract the
     initialisation code from it.
  *)
  | SYMDEF_root _ -> () 
  | SYMDEF_library -> () 
  | SYMDEF_module -> ()
    (*
    add_bsym true_parent (bbdcl_module ())
    *)

  | SYMDEF_typeclass ->
    add_bsym true_parent (bbdcl_typeclass ([], bvs))

  | SYMDEF_virtual_type ->
    add_bsym true_parent (bbdcl_virtual_type bvs)
 
  | SYMDEF_reduce reds -> assert false
(* 
    let reds =
      List.map (fun (ivs,ps,e1,e2) ->
        let bps = bind_basic_ps ps in
        let be1 = be e1 in
        let be2 = be e2 in
        let bvs = map (fun (s,i,tp) -> s,i,Flx_btype.bmt "Flx_bbind.2" tp) (fst ivs) in
        bvs,bps,be1,be2
      )
      reds
    in
    let r = sym.Flx_sym.id,reds in
    state.reductions := r :: !(state.reductions);

    if state.print_flag then
      print_endline ("//bound reduction  " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" );

    add_bsym true_parent (bbdcl_reduce ())
*)

  | SYMDEF_axiom (ps,e1) ->
    let bps = bindps ps in
    let be1 = match e1 with
      | Predicate e -> `BPredicate (be e)
      | Equation (l,r) -> `BEquation (be l, be r)
    in
    state.axioms :=  (
      sym.Flx_sym.id,
      sym.Flx_sym.sr,
      sym_parent,
      Axiom,
      bvs,
      bps,
      be1) :: !(state.axioms);

    if state.print_flag then
      print_endline ("//bound axiom " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^ print_bvs bvs);

    add_bsym true_parent (bbdcl_axiom ())

  | SYMDEF_lemma (ps,e1) ->
    let bps = bindps ps in
    let be1 = match e1 with
      | Predicate e -> `BPredicate (be e)
      | Equation (l,r) -> `BEquation (be l, be r)
    in
    state.axioms := (
      sym.Flx_sym.id,
      sym.Flx_sym.sr,
      sym_parent,
      Lemma,
      bvs,
      bps,
      be1) :: !(state.axioms);

    if state.print_flag then
      print_endline ("//bound lemma " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^ print_bvs bvs);

    add_bsym true_parent (bbdcl_lemma ())

  | SYMDEF_function (ps,rt,effects,props,exes) ->

(*
if sym.Flx_sym.id = "f" then 
print_endline ("Flx_bbind: Binding function " ^ sym.Flx_sym.id);
if sym.Flx_sym.id = "join" then 
  print_endline ("join: precalculated bvs = " ^ Flx_print.string_of_bvs bvs);
if sym.Flx_sym.id = "join" then 
    print_endline (" ... Binding parameters");
*)
    let bps = bindps ps in
(*
if sym.Flx_sym.id = "f" then begin
    print_endline (" ... DONE Binding parameters");
    print_endline ("Input unbound parameters: " ^ string_of_parameters ps);
    print_endline ("Output  bound parameters: " ^ string_of_bparameters bsym_table bps);
end;
*)

(*
if sym.Flx_sym.id = "join" then 
print_endline ("join: binding return type");
*)
    (* We don't need to bind the intermediary type. *)
    let brt = bt' rt in
(*
if sym.Flx_sym.id = "join" then 
print_endline ("join: bound return type");
*)
(*
print_endline ("Flx_bbind: Calculate return type " ^ string_of_typecode rt ^
  " ==> " ^ sbt bsym_table brt);
*)
    let beffects = bt' effects in
(*
if sym.Flx_sym.id = "hhhhh" then
print_endline ("Effects = " ^ Flx_btype.st beffects);
*)
(*
if sym.Flx_sym.id = "join" then 
print_endline ("Join: binding executable instructions");
*)
    let brt, bbexes = bexes exes brt symbol_index bvs in
    let bbdcl = bbdcl_fun (props,bvs,bps,brt,beffects,bbexes) in
    let d = Flx_bparams.get_btype bps in
    let ft =
      if mem `Cfun props
      then btyp_cfunction (d,brt)
      else btyp_effector (d,beffects,brt)
    in

    (* Cache the type of the function. *)
    if not (Hashtbl.mem state.ticache symbol_index) then begin
      let t = Flx_fold.fold bsym_table state.counter ft in
      if debug then 
        print_endline ("Flx_bbind: Adding type of index " ^ si symbol_index ^ " to cache, type=" ^ Flx_btype.st t);
      Hashtbl.add state.ticache symbol_index t
    end;

    if (* sym.Flx_sym.id = "f" || *) state.print_flag then begin
      print_endline ("//bound function " ^ qname ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table ft)
    end;

    add_bsym true_parent bbdcl

  | SYMDEF_parameter (k,_) ->
(*
print_endline "BINDING PARAMETER";
*)
    begin match sym_parent with
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
        | `POnce -> bbdcl_val (bvs,t,`Once)
        | `PVal ->  
           if Flx_btype.contains_uniq t then begin
             print_endline ("Flx_bbind: WARNING: Parameter " ^ sym.Flx_sym.id ^ 
               " type " ^ sbt bsym_table t ^
               " is or contains uniq specified or defaults to val, var recommended");
             print_endline (Flx_srcref.long_string_of_src sym.Flx_sym.sr)
(*
             clierr sym.Flx_sym.sr ("Parameter " ^ sym.Flx_sym.id ^ 
               " is or contains uniq specified or defaults to val, var is required")
*)
           end;
           bbdcl_val (bvs,t,`Val)
        | `PVar -> bbdcl_val (bvs,t,`Var)
        in
        Hashtbl.add state.varmap symbol_index t;

        if state.print_flag then
          print_endline ("//bound val " ^ sym.Flx_sym.id ^ "<" ^
            string_of_bid symbol_index ^ ">" ^
            print_bvs bvs ^ ":" ^ sbt bsym_table t);

        add_bsym true_parent bbdcl

      | _ ->
        failwith ("[bbind_sym] expected parameter to have function or " ^
          "functor parent")
    end

  | SYMDEF_label s ->
(*
print_endline ("flx_bind: Adding label " ^ s ^ " index " ^ string_of_int symbol_index ^ " parent " ^
  (match true_parent with | None -> "None" | Some x -> string_of_int x));
  print_endline ("Current srcref = " ^ Flx_srcref.short_string_of_src sym.sr);
*)
    add_bsym true_parent (bbdcl_label s) 

  | SYMDEF_const_ctor (uidx,ut,ctor_idx,vs') ->
    (*
    print_endline ("Binding const ctor " ^ sym.Flx_sym.id);
    *)
    let t = type_of_index symbol_index in
    let ut = bt ut in
    let btraint = bind_type_constraint vs' in
    let evs = map (fun (s,i,k) -> s,i, Flx_btype.bmt "Flx_bbind.2" k) (fst vs') in

    if state.print_flag then
      print_endline ("//bound const ctor " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">:" ^ sbt bsym_table t);

    add_bsym None (bbdcl_const_ctor (bvs,uidx,ut,ctor_idx,evs,btraint))

  | SYMDEF_nonconst_ctor (uidx,ut,ctor_idx,vs',argt) ->
    (*
    print_endline ("Binding non const ctor " ^ sym.Flx_sym.id);
    *)
    let t = type_of_index symbol_index in
    let argt = bt argt in
    let ut = bt ut in
    let btraint = bind_type_constraint vs' in
    let evs = map (fun (s,i,k) -> s,i, Flx_btype.bmt "Flx_bbind.4" k) (fst vs') in

    if state.print_flag then
      print_endline ("//bound nonconst ctor " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">:" ^ sbt bsym_table t);

    add_bsym None (bbdcl_nonconst_ctor (bvs,uidx,ut,ctor_idx,argt,evs,btraint))

  | SYMDEF_val t ->
    let t = 
      try type_of_index symbol_index 
      with GadtUnificationFailure ->
(*
        print_endline ("GADT UNIFICATION FAILURE BINDING TYPE OF VARIABLE " ^ sym.Flx_sym.id);
*)
        btyp_void ()
    in
    if Flx_btype.contains_uniq t then begin
      print_endline ("Flx_bbind: WARNING: Local val " ^ sym.Flx_sym.id ^ " type " ^ 
        sbt bsym_table t ^
        " is or contains uniq, var is recommended");
      print_endline (Flx_srcref.long_string_of_src sym.Flx_sym.sr)
(*
      clierr sym.Flx_sym.sr ("Local val " ^ sym.Flx_sym.id ^ 
        " is or contains uniq, var is required")
*)
    end;

    if state.print_flag then
      print_endline ("//bound val " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table t);

    add_bsym true_parent (bbdcl_val (bvs, t, `Val))

  | SYMDEF_once t ->
    let t = 
      try type_of_index symbol_index 
      with GadtUnificationFailure ->
(*
        print_endline ("GADT UNIFICATION FAILURE BINDING TYPE OF VARIABLE " ^ sym.Flx_sym.id);
*)
        btyp_void ()
    in

    if state.print_flag then
      print_endline ("//bound val " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table t);

    add_bsym true_parent (bbdcl_val (bvs, t, `Once))

  | SYMDEF_var t ->
    let t = type_of_index symbol_index in

    if state.print_flag then
      print_endline ("//bound var " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table t);

    add_bsym true_parent (bbdcl_val (bvs, t, `Var))

  | SYMDEF_ref t ->
    let t = type_of_index symbol_index in

    if state.print_flag then
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
    if not (Hashtbl.mem state.ticache symbol_index) then begin
      (* HACK! *)
if debug then
print_endline ("Flx_bbind: Adding type of index " ^ si symbol_index ^ " to cache, type=" ^ Flx_btype.st brt);
      Hashtbl.add state.ticache symbol_index brt
    end;

    if state.print_flag then
      print_endline ("//bound lazy " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table brt);
    let beffects = btyp_unit () in
    add_bsym true_parent (bbdcl_fun (props,bvs,Flx_bparams.unit_bparams,brt,beffects,bbexes))

  | SYMDEF_const (props,t,ct,reqs) ->
    let t = type_of_index symbol_index in
    let reqs = bind_reqs reqs in

    if state.print_flag then
      print_endline ("//bound const " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table t);

    add_bsym true_parent (bbdcl_external_const (props,bvs,t,ct,reqs))

  | SYMDEF_fun (props,ts,ret,ct,reqs,prec) ->
    let ts = map bt ts in
    let bret = bt ret in

    (* Cache the type of the function. *)
    if not (Hashtbl.mem state.ticache symbol_index) then begin
      let t = Flx_fold.fold bsym_table state.counter (btyp_function (btyp_tuple ts, bret)) in
if debug then
print_endline ("Flx_bbind: Adding type of index " ^ si symbol_index ^ " to cache, type=" ^ Flx_btype.st t);
      Hashtbl.add state.ticache symbol_index t
    end;

    if state.print_flag then 
    begin
      let atyp = btyp_tuple ts in
      print_endline ("//bound fun " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^
        print_bvs bvs ^ ":" ^ sbt bsym_table (btyp_function (atyp, bret)))
    end;

    add_bsym true_parent (bbdcl_external_fun (
      props,
      bvs,
      ts,
      bret,
      (bind_reqs reqs),
      prec,
      `Code ct))

  | SYMDEF_callback (props,ts_orig,ret,reqs) ->
(*
print_endline ("Binding callback " ^ sym.Flx_sym.id ^ " index=" ^ string_of_bid symbol_index);
*)
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

    (* Cache the type of the callback. *)
    (* JS: HUMM .. why is the counter passed to fold? *)
    (* Ans: because folding requires alpha conversion which requires
     * fresh names for type variables
     *)
    if not (Hashtbl.mem state.ticache symbol_index) then begin
      let t = Flx_fold.fold bsym_table state.counter (btyp_cfunction (btyp_tuple ts_cf, bret)) in
if debug then
print_endline ("Flx_bbind: Adding type of index " ^ si symbol_index ^ " to cache, type=" ^ Flx_btype.st t);
      Hashtbl.add state.ticache symbol_index t
    end;

    if state.print_flag then begin
      let atyp = btyp_tuple ts_cf in
      print_endline ("//bound callback fun " ^ sym.Flx_sym.id ^ "<" ^
        string_of_bid symbol_index ^ ">" ^ print_bvs bvs ^ ":" ^
        sbt bsym_table (btyp_function (atyp, bret)))
    end;

    add_bsym true_parent (bbdcl_external_fun (
      props,
      bvs,
      ts_cf,
      bret,
      (bind_reqs reqs),
      prec,
      `Callback (ts_c,!client_data_pos)))

  | SYMDEF_union (cs) ->
    if state.print_flag then
      print_endline ("//Binding union " ^ si symbol_index ^ " --> " ^ sym.Flx_sym.id);
    let ut = btyp_inst (
      symbol_index, 
      List.map (fun (s,i,k) -> btyp_type_var (i,k)) bvs, 
      Flx_kind.KIND_type) 
    in
    let cs' = List.map (fun (n,v,vs',d,c,gadt) -> 
      let evs = List.map (fun (s,i,k) -> s,i,Flx_btype.bmt "Flx_bbind.7" k) (fst vs') in
      n, v, evs, bt d, bt c, gadt
    ) cs 
    in
    add_bsym None (bbdcl_union (bvs, cs'))

  | SYMDEF_struct cs ->
    if state.print_flag then 
      print_endline ("//Binding struct " ^ si symbol_index ^ " --> " ^ sym.Flx_sym.id);
    let cs' = List.map (fun (n,t) -> n, bt t) cs in
    add_bsym None (bbdcl_struct (bvs, cs'))

  | SYMDEF_cstruct (cs,reqs) ->
    if state.print_flag then 
      print_endline ("//Binding cstruct " ^ si symbol_index ^ " --> " ^ sym.Flx_sym.id);
    let cs' = List.map (fun (n,t) -> n, bt t) cs in
    let breqs = bind_reqs reqs in 
    add_bsym None (bbdcl_cstruct (bvs, cs', breqs))

  | SYMDEF_instance qn ->
    (*
    print_endline "INSTANCE";
    *)
    let (k:entry_kind_t),(ts: typecode_t list) = luqn qn in
    let k = sye k in

    (* Make sure the typeclass is in the symbol table first. *)
    let typeclass_parent, typeclass_sym = Flx_sym_table.find_with_parent
      state.sym_table
      k
    in
(*
print_endline (" &&&&&& SYMDEF_instance calling BBIND_SYMBOL");
*)
    bbind_symbol state bsym_table k typeclass_parent typeclass_sym;

    (*
    print_endline ("binding ts = " ^ catmap "," string_of_typecode ts);
    *)
    let ts = map bt ts in
    (*
    print_endline "DOne ..";
    *)
    (*
    print_endline ("Flx_bbind: adding instance with constraint " ^ sbt bsym_table bcons);
    *)
    add_bsym true_parent (bbdcl_instance ([], bvs, bcons, k, ts))

  | SYMDEF_inherit _ -> ()
  | SYMDEF_inherit_fun _ -> ()

  | SYMDEF_abs (quals,ct,reqs)->
    if state.print_flag then
      print_endline ("//Binding abstract primitive type " ^ si symbol_index ^ " ->  " ^ sym.Flx_sym.id);
    let reqs = bind_reqs reqs in
    let bquals = bind_quals quals in
    add_bsym None (bbdcl_external_type (bvs, bquals, ct, reqs))

  | SYMDEF_newtype t ->
    let t = bt t in
    add_bsym None (bbdcl_newtype (bvs, t))

  | SYMDEF_type_alias t ->
(*
print_endline ("Binding type alias .. ");
*)
    if get_structural_typedefs state then begin 
(*
print_endline ("NOT **** Adding typedef " ^ sym.Flx_sym.id ^"<"^ si symbol_index ^ "> = " ^ Flx_print.string_of_typecode t ^ " to bsym_table");
*)
       () 
    end else begin
(*
print_endline ("TRYING TO BIND typedef " ^ sym.Flx_sym.id ^"<"^ si symbol_index ^ "> = " ^ Flx_print.string_of_typecode t );
*)
    let t = 
      try bt t 
      with exn ->
        print_endline ("BINDING typedef " ^ sym.Flx_sym.id ^"<"^ si symbol_index ^ "> = " ^ 
          Flx_print.string_of_typecode t ^ "  FAILED with " ^ Printexc.to_string exn);
        raise exn
    in
(*
print_endline ("Adding typedef " ^ sym.Flx_sym.id ^"<"^ si symbol_index ^ "> = " ^ Flx_btype.st t ^ " to bsym_table");
*)
    add_bsym None (bbdcl_nominal_type_alias (bvs, t))
   end

  | SYMDEF_instance_type t ->
    let t = bt t in
    add_bsym true_parent (bbdcl_instance_type (bvs, t))


  | SYMDEF_insert (ct,ikind,reqs) ->
    if state.print_flag then 
      print_endline ("//Binding insertion string " ^ si symbol_index ^ 
       " --> " ^ sym.Flx_sym.id^ ":"^ 
       string_of_ikind ikind ^"="^ string_of_code_spec ct);
    let reqs = bind_reqs reqs in
    add_bsym true_parent (bbdcl_external_code (bvs, ct, ikind, reqs))
  end
  (*
  ;
  print_endline ("BINDING " ^ name ^ "<" ^ si i ^ "> COMPLETE");
  flush stdout
  *)
  end

let rec fix_typeofs state bsym_table t = 
  match t with
  | BTYP_typeof (symbol_index,expr) ->
    let env = Flx_lookup.build_env
      state.lookup_state bsym_table (Some symbol_index)
    in
    let be = Flx_lookup.bind_expression
      state.lookup_state bsym_table env expr
    in
    let typ = snd be in
    typ
  | _ -> Flx_btype.map ~f_btype:(fix_typeofs state bsym_table) t


let bbind (state:Flx_bbind_state.bbind_state_t) start_counter ref_counter bsym_table =
(*
print_endline ("Flx_bbind.bbind *********************** ");
*)
  (* loop through all counter values [HACK] to get the indices in sequence, AND,
   * to ensure any instantiations will be bound, (since they're always using the
   * current value of state.counter for an index. Note that in the process of
   * binding new symbols can be added to the end of the table, so the terminating
   * index is not known in advance. It had better converge!
   *)
  (* PASS 0, TYPEDEFS ONLY *)

(*
print_endline ("\n====================\nSetting type aliases to nominal\n===============\n");
*)
  let saved_env_cache = Hashtbl.copy state.lookup_state.Flx_lookup_state.env_cache in
  let saved_visited = Hashtbl.copy state.visited in

(*
  let bsym_table_dummy = Flx_bsym_table.create_fresh () in 
*)
  let bsym_table_dummy = bsym_table in
  set_nominal_typedefs state;
  let counter = ref start_counter in
  while !counter < !ref_counter do
    let i = !counter in
    if Hashtbl.mem state.visited i then 
    begin 
(*
      print_endline ("Skipping already processed typedef " ^ string_of_int i);
*)
     () 
    end
    else
    begin match
      try Some (Flx_sym_table.find_with_parent state.sym_table i)
      with Not_found -> None
    with
    | None -> (* print_endline "bbind: Symbol not found"; *) ()
    | Some (parent, sym) ->
(*
print_endline ("[flx_bbind] bind_symbol " ^ sym.Flx_sym.id ^ "??");
*)
      begin match sym.Flx_sym.symdef with
      | Flx_types.SYMDEF_type_alias  t
      ->
begin try
(*
print_endline ("Binding typedef " ^ sym.Flx_sym.id ^ "<"^string_of_int i^"> = " ^ string_of_typecode t);
*)
        begin try bbind_symbol state bsym_table_dummy i parent sym
        with Not_found ->
          try match hfind "bbind" state.sym_table i with { Flx_sym.id=id } ->
            print_endline ("Binding error, Not_found thrown binding " ^ id ^ " index " ^
              string_of_bid i ^ " parent " ^ (match parent with | None -> "NONE" | Some p -> string_of_int p))
          with Not_found ->
            failwith ("Binding error, Not_found thrown binding unknown id with index " ^ string_of_bid i)
        end
with _ -> ()
end
      | _ -> ()
      end
    end;
    incr counter
  done
  ;

(*
  print_endline ("\n=====================\n TYPEDEFS before expansion\n=====================\n");
  Flx_bsym_table.iter (fun bid parent bsym ->
    let bbdcl = Flx_bsym.bbdcl bsym in
    match bbdcl with
    | BBDCL_nominal_type_alias _ -> print_endline ("typedef " ^ string_of_int bid ^ " -> " ^
      Flx_print.string_of_bbdcl bsym_table_dummy bbdcl bid)
    | _ -> ()
  ) bsym_table_dummy;  
  print_endline ("\n==========================================\n");
*)
  Flx_bsym_table.iter (fun bid parent bsym ->
    let bbdcl = Flx_bsym.bbdcl bsym in
    let sr = Flx_bsym.sr bsym in
    match bbdcl with
    | BBDCL_nominal_type_alias (bvs,t) -> 
      
      let r = Flx_expand_typedef.expand bsym_table state.counter sr t in
      let b = bbdcl_structural_type_alias (bvs, r) in 
      Flx_bsym_table.update_bbdcl bsym_table_dummy bid b
 
    | _ -> ()
  ) bsym_table_dummy;

(*
  print_endline ("\n=====================\n TYPEDEFS after expansion\n=====================\n");
  Flx_bsym_table.iter (fun bid parent bsym ->
    let bbdcl = Flx_bsym.bbdcl bsym in
    match bbdcl with
    | BBDCL_structural_type_alias _ -> print_endline ("typedef " ^ string_of_int bid ^ " -> " ^
      Flx_print.string_of_bbdcl bsym_table_dummy bbdcl bid)
    | _ -> ()
  ) bsym_table_dummy;  
*)

(*
  print_endline ("\n=====================\n VAR CACHE (function codomains) \n=====================\n");
  Hashtbl.iter (fun i t ->
    print_endline ("type var " ^ string_of_int i ^ " -> " ^ Flx_btype.st t);
  )
  state.varmap;
*)
(*
  Hashtbl.clear state.varmap;
  Hashtbl.clear state.virtual_to_instances;
  Hashtbl.clear state.instances_of_typeclass;
  Hashtbl.clear state.lookup_state.generic_cache;
*)
(*
  print_endline ("\n=====================\nTYPE CACHE \n=====================\n");
  Hashtbl.iter (fun i t ->
    print_endline ("type of index " ^ string_of_int i ^ " -> " ^ Flx_btype.st t);
  )
  state.ticache;
*)
 
  state.visited <-saved_visited;
  state.lookup_state.Flx_lookup_state.env_cache <- saved_env_cache;

(*
  (* copy the typedefs into the main symbol table *)
   Flx_bsym_table.iter (fun bid parent bsym ->
    let bbdcl = Flx_bsym.bbdcl bsym in
    match bbdcl with
    | BBDCL_structural_type_alias _ -> 
      Flx_bsym_table.add bsym_table bid parent bsym;
      Hashtbl.add state.visited bid ()
    | _ -> 
      print_endline ("Copy typedefs: Not copying " ^ string_of_int bid);
      assert false;
      ()
  ) bsym_table_dummy;  
*)

(*
  print_endline ("\n===================\nSetting type aliases to structura\n=======================\n");
*)

  (* PASS 1, TYPE ONLY *)
  set_structural_typedefs state;

(*
print_endline ("Fixing typeofs");
*)
  Flx_bsym_table.iter (fun bid parent bsym ->
    let bbdcl = Flx_bsym.bbdcl bsym in
    let sr = Flx_bsym.sr bsym in
    match bbdcl with
    | BBDCL_structural_type_alias (bvs,t) -> 
      let r = fix_typeofs state bsym_table t in 
      let b = bbdcl_structural_type_alias (bvs, r) in 
      Flx_bsym_table.update_bbdcl bsym_table bid b
 
    | _ -> ()
  ) bsym_table_dummy;
(*
print_endline ("Done fixing typeofs");
*)

  let counter = ref start_counter in
  while !counter < !ref_counter do
    let i = !counter in
    begin match
      try Some (Flx_sym_table.find_with_parent state.sym_table i)
      with Not_found -> None
    with
    | None -> (* print_endline "bbind: Symbol not found"; *) ()
    | Some (parent, sym) ->
(*
print_endline ("[flx_bbind] bind_symbol " ^ sym.Flx_sym.id ^ "??");
*)
      begin match sym.Flx_sym.symdef with
      | Flx_types.SYMDEF_union _
      | Flx_types.SYMDEF_struct _
      | Flx_types.SYMDEF_cstruct _ 
      | Flx_types.SYMDEF_abs _ 
      | Flx_types.SYMDEF_const_ctor _
      | Flx_types.SYMDEF_nonconst_ctor _
      | Flx_types.SYMDEF_newtype _
      ->
        begin try bbind_symbol state bsym_table i parent sym
        with Not_found ->
          try match hfind "bbind" state.sym_table i with { Flx_sym.id=id } ->
            failwith ("Binding error, Not_found thrown binding " ^ id ^ " index " ^
              string_of_bid i ^ " parent " ^ (match parent with | None -> "NONE" | Some p -> string_of_int p))
          with Not_found ->
            failwith ("Binding error, Not_found thrown binding unknown id with index " ^ string_of_bid i)
        end
      | _ -> ()
      end
    end;
    incr counter
  done
  ;

  (* PASS 2, SUBTYPE COERCIONS ONLY *)
  let counter = ref start_counter in
  while !counter < !ref_counter do
    let i = !counter in
    begin match
      try Some (Flx_sym_table.find_with_parent state.sym_table i)
      with Not_found -> None
    with
    | None -> (* print_endline "bbind: Symbol not found"; *) ()
    | Some (parent, sym) ->
(*
print_endline ("[flx_bbind] bind_symbol " ^ sym.Flx_sym.id ^ "??");
*)
      let sr = sym.Flx_sym.sr in
      let vs = fst sym.Flx_sym.vs in
      begin match sym.Flx_sym.symdef with
      | SYMDEF_function (params,ret,effect,props,_) when List.mem `Subtype props ->
        if List.length vs <> 0 then
          clierr sr ("   Improper subtype, no type variables allowed, got " ^
           string_of_int (List.length vs))
        else
        let dom, cod = 
          let ps = fst params in
          begin match ps with
          | Satom (sr,kind,id,typ,initopt) -> typ,ret 
          | _ ->
             clierr sr ("Improper subtype, only one parameter allowed, got " ^ 
               string_of_paramspec_t ps)
          end
        in
        begin
          begin
            try bbind_symbol state bsym_table i parent sym
            with Not_found ->
              try match hfind "bbind" state.sym_table i with { Flx_sym.id=id } ->
                failwith ("Binding error, Not_found thrown binding " ^ id ^ " index " ^
                  string_of_bid i ^ " parent " ^ (match parent with | None -> "NONE" | Some p -> string_of_int p))
              with Not_found ->
                failwith ("Binding error, Not_found thrown binding unknown id with index " ^ string_of_bid i)
          end;
          (* now, bind the types dom,cod! We cheat, and just lookup the cache*)
(*
          print_endline ("bind dom/cod");
*)
          let ft =
             try Hashtbl.find state.ticache i 
             with Not_found -> failwith ("WOOPS, expected type of subtype function in cache");
          in
(*
          print_endline ("Type of function " ^ string_of_int i ^ " is " ^ sbt bsym_table ft);
*)
          match ft with
          | BTYP_function (BTYP_inst (dom,[],_),BTYP_inst (cod,[],_)) ->
(*
            print_endline ("Domain index = " ^ string_of_int dom ^ " codomain index = " ^ string_of_int cod);
*)
            Flx_bsym_table.add_supertype bsym_table ((cod,dom),i)
 
          | _ -> clierr sr ("Subtype specification requires nonpolymorphic nominal typed function")
        end

      | SYMDEF_fun (props,ps,ret,_,_,_) when List.mem `Subtype props ->
        if List.length vs <> 0 then
          print_endline ("   Improper subtype, no type variables allowed, got " ^
           string_of_int (List.length vs))
        else
        let dom, cod = 
          begin match ps with
          | [typ] -> typ,ret
          | _ ->
             clierr sr ("Improper subtype, only one parameter allowed, got " ^ 
               string_of_int (List.length ps))
          end
        in
        begin 
          begin
            try bbind_symbol state bsym_table i parent sym
            with Not_found ->
              try match hfind "bbind" state.sym_table i with { Flx_sym.id=id } ->
                failwith ("Binding error, Not_found thrown binding " ^ id ^ " index " ^
                  string_of_bid i ^ " parent " ^ (match parent with | None -> "NONE" | Some p -> string_of_int p))
              with Not_found ->
                failwith ("Binding error, Not_found thrown binding unknown id with index " ^ string_of_bid i)
          end;
          (* now bind the types dom,cod *)
(*
          print_endline ("bind dom/cod");
*)
          let ft =
             try Hashtbl.find state.ticache i 
             with Not_found -> failwith ("WOOPS, expected type of subtype function in cache");
          in
(*
          print_endline ("Type of function " ^ string_of_int i ^ " is " ^ sbt bsym_table ft);
*)
          match ft with
          | BTYP_function (BTYP_inst (dom,[],_),BTYP_inst (cod,[],_)) ->
(*
            print_endline ("Domain index = " ^ string_of_int dom ^ " codomain index = " ^ string_of_int cod);
*)
            Flx_bsym_table.add_supertype bsym_table ((cod,dom),i)
          | _ -> clierr sr ("Subtype specification requires nonpolymorphic nominal typed function")
        end
      | _ -> ()
      end (* symdef *)
    end; (* parent *)
    incr counter
  done
  ;

  (* PASS 3, NON DEFERRED FUNCTIONS *)
  let defered = ref [] in
  let counter = ref start_counter in
  while !counter < !ref_counter do
    let i = !counter in
    begin match
      try Some (Flx_sym_table.find_with_parent state.sym_table i)
      with Not_found -> None
    with
    | None -> (* print_endline "bbind: Symbol not found"; *) ()
    | Some (parent, sym) ->
(*
print_endline ("[flx_bbind] bind_symbol " ^ sym.Flx_sym.id ^ "??");
*)
      begin match sym.Flx_sym.symdef with
      | Flx_types.SYMDEF_function ((Satom (psr,kind,pid,TYP_defer _,_),None),ret,effects,props,exes) ->
print_endline ("[flx_bbind] bind_symbol FUNCTION " ^ sym.Flx_sym.id ^ " .. DEFERED");
        defered := i :: !defered
      | Flx_types.SYMDEF_parameter (kind,TYP_defer _) ->
print_endline ("[flx_bbind] bind_symbol PARAMETER " ^ sym.Flx_sym.id ^ " .. DEFERED");
        defered := i :: !defered
 
      | _ -> 
(*
print_endline ("[flx_bbind] bind_symbol " ^ sym.Flx_sym.id ^ " .. BINDING: calling BBIND_SYMBOL");
print_endline ("Binding symbol " ^ symdef.Flx_sym.id ^ "<" ^ si i ^ ">"); 
*)
      try bbind_symbol state bsym_table i parent sym
      with Not_found ->
        try match hfind "bbind" state.sym_table i with { Flx_sym.id=id } ->
          failwith ("Binding error, Not_found thrown binding " ^ id ^ " index " ^
            string_of_bid i ^ " parent " ^ (match parent with | None -> "NONE" | Some p -> string_of_int p))
        with Not_found ->
          failwith ("Binding error, Not_found thrown binding unknown id with index " ^ string_of_bid i)
      end
    end;
    incr counter
  done
  ;

  (* PASS 4 DEFERRED FUNCTIONS *)
if (List.length (!defered) <> 0) then begin
print_endline ("DEFERED PROCESSING STARTS");

  List.iter (fun i ->
    begin match
      try Some (Flx_sym_table.find_with_parent state.sym_table i)
      with Not_found -> None
    with
    | None -> (* print_endline "bbind: Symbol not found"; *) ()
    | Some (parent, sym) ->
print_endline ("[flx_bbind] DEFERED bind_symbol " ^ sym.Flx_sym.id ^ "?? calling BBIND_SYMBOL");
      try bbind_symbol state bsym_table i parent sym
      with Not_found ->
        try match hfind "bbind" state.sym_table i with { Flx_sym.id=id } ->
          failwith ("Binding error, Not_found thrown binding " ^ id ^ " index " ^
            string_of_bid i)
        with Not_found ->
          failwith ("Binding error, Not_found thrown binding unknown id with index " ^ string_of_bid i)
    end
  )
  (!defered)
  ;
print_endline ("DEFERED PROCESSING ENDS");
end


