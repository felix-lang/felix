open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_typing2
open Flx_lookup
open Flx_mbind
open Flx_unify
open Flx_exceptions
open List
open Flx_maps
open Flx_lookup_state

type bexe_state_t = {
  counter: Flx_types.bid_t ref;
  sym_table: Flx_sym_table.t;
  lookup_state: Flx_lookup_state.lookup_state_t;
  env: Flx_mtypes2.env_t;
  id: string;
  parent: Flx_types.bid_t option;
  parent_vs: Flx_types.bvs_t;
  mutable ret_type: Flx_btype.t;
  mutable reachable: bool;
  mutable return_count: int;
  mutable proc_return_count: int;
}

let do_unify state bsym_table a b =
  Flx_do_unify.do_unify state.counter (Flx_lookup_state.get_varmap state.lookup_state)  state.sym_table bsym_table a b

let make_bexe_state ?parent ?(env=[]) counter sym_table lookup_state parent_vs ret_type =
  let id =
    match parent with
    | None -> ""
    | Some index ->
        let symbol = Flx_sym_table.find sym_table index in
        symbol.Flx_sym.id
  in
  {
    counter = counter;
    sym_table = sym_table;
    lookup_state = lookup_state;
    env = env;
    id = id;
    parent = parent;
    parent_vs = parent_vs;
    ret_type = ret_type;
    reachable = true;
    return_count = 0;
    proc_return_count = 0;
  }

let hfind msg h k =
  try Flx_sym_table.find h k
  with Not_found ->
    print_endline ("flx_bexe Flx_sym_table.find failed " ^ msg);
    raise Not_found

(*
  THIS IS A DUMMY BOUND SYMBOL TABLE
  REQUIRED FOR THE PRINTING OF BOUND EXPRESSIONS
*)
let bsym_table = Flx_bsym_table.create ()

let rec check_if_parent sym_table child parent =
  if child = parent then true
  else
    let parent = Flx_sym_table.find_parent sym_table child in
    match parent with
    | Some parent -> check_if_parent sym_table child parent
    | None -> false

let cal_call state bsym_table sr ((be1,t1) as tbe1) ((_,t2) as tbe2) =
  let be i e =
    bind_expression
      state.lookup_state 
      bsym_table
      (build_env state.lookup_state bsym_table (Some i))
      e
  in
  let genargs t =
    if type_match bsym_table state.counter t t2
    then
      (
        (*
        match p with
        | BEXPR_closure (i,ts) ->
          begin match hfind "bexe" sym_table i with
          | { Flx_sym.symdef=SYMDEF_fun _ }
          | { Flx_sym.symdef=SYMDEF_callback _ }
            ->
            BEXE_call_prim (sr,i,ts,tbe2)

          | { Flx_sym.symdef=SYMDEF_function _} ->
            BEXE_call_direct (sr,i,ts,tbe2)

          | _ -> assert false
          end
        | _ ->
        *)
          (sr,tbe1,tbe2)
      )
    else
    begin
      let reorder =
        match be1 with
        | BEXPR_closure (i,ts) ->
          begin match t2 with
          (* a bit of a hack .. *)
          | BTYP_record _ | BTYP_tuple [] ->
            let rs = match t2 with
              | BTYP_record ("",rs) -> rs
              | BTYP_tuple [] -> []
              | _ -> assert false
            in
            begin let pnames = match hfind "bexe" state.sym_table i with
            | { Flx_sym.symdef=SYMDEF_function (ps,_,_,_)} ->
              map (fun (_,name,_,d)->
                name,
                match d with None -> None | Some e -> Some (be i e)
              ) (fst ps)
            | _ -> assert false
            in
            let n = length rs in
            let rs= sort (fun (a,_) (b,_) -> compare a b) rs in
            let rs = map2 (fun (name,t) j -> name,(j,t)) rs (nlist n) in
            try Some (map
              (fun (name,d) ->
                try (match assoc name rs with
                | j,t-> bexpr_get_n t j tbe2)
                with Not_found ->
                match d with
                | Some d ->d
                | None -> raise Not_found
              )
              pnames
            )
            with Not_found -> None
            end

          | _ -> None
          end
        | _ -> None
      in
      let x2 = match reorder with
        | Some xs ->
          begin match xs with
          | [x]-> x
          | _ -> bexpr_tuple (btyp_tuple (map snd xs)) xs
          end
        | None ->
          clierr sr
          (
            "[cal_call] Procedure " ^
            sbe bsym_table tbe1 ^
            "\nof type " ^
            sbt bsym_table t1 ^
            "\napplied to argument " ^
            sbe bsym_table tbe2 ^
            "\n of type " ^
            sbt bsym_table t2 ^
            "\nwhich doesn't agree with parameter type\n" ^
            sbt bsym_table t
          )
      in (sr,tbe1,x2)
    end
  in
  match unfold t1 with

  (* special handling of non-returning function. Instead of returning
     void we return type any, 
  *)
  | BTYP_cfunction (t, BTYP_fix (0,_))
  | BTYP_function (t, BTYP_fix (0,_)) 
    ->
    let a = genargs t in
    bexe_jump a

  | BTYP_cfunction (t, BTYP_void)
  | BTYP_function (t, BTYP_void) 
    ->
    let a = genargs t in
    bexe_call a

  | _ ->
    clierr sr ("[cal_call] call non procedure, "^
    sbe bsym_table tbe1
    ^"\ntype=" ^ sbt bsym_table t1)

let cal_loop sym_table sr ((p,pt) as tbe1) ((_,argt) as tbe2) this =
  match unfold pt with
  | BTYP_function (t, BTYP_void) ->
    if t = argt
    then
      match p with
      | BEXPR_closure (i,ts) ->
        if check_if_parent sym_table i this
        then
          bexe_call (sr,(p,pt),tbe2)
          (*
          BEXE_call_direct (sr,i, ts, tbe2)
          *)
        else
          clierr sr
          "[cal_loop] Loop target must be self or parent"

      | _ ->
        clierr sr (
          "[cal_loop] Expected procedure closure, got "^
          sbe bsym_table (p,pt)
        )
    else
      clierr sr
      (
        "[cal_loop] Procedure " ^
        sbe bsym_table tbe1 ^
        "\nof type " ^
        sbt bsym_table pt ^
        "\napplied to argument " ^
        sbe bsym_table tbe2 ^
        "\n of type " ^
        sbt bsym_table argt ^
        "\nwhich doesn't agree with parameter type\n" ^
        sbt bsym_table t
      )

  | _ ->
    clierr sr ("[cal_loop] loop to non procedure, "^
    sbe bsym_table (p,pt)
    ^"\ntype=" ^ sbt bsym_table pt)

exception Found of int

let print_vs vs =
  catmap "," (fun (s,i) -> s ^ "->" ^ string_of_bid i) vs

let rec bind_exe state bsym_table handle_bexe (sr, exe) init =
  let be e =
    bind_expression
      state.lookup_state
      bsym_table
      state.env
      e
  in
  let lun sr n =
    lookup_name_in_env
      state.lookup_state
      bsym_table
      state.env
      sr
      n
  in
(*
  print_endline ("EXE="^string_of_exe 1 exe);
*)
(*
  if not state.reachable then
  begin
    match exe with
    | EXE_label _ -> ()
    | EXE_comment _ -> ()
    | EXE_nop _ -> ()
    | EXE_catch _ -> ()
    | EXE_endtry -> ()
    | _ -> print_endline
      (
        "WARNING: Unreachable code in " ^ state.id ^ ": " ^
        string_of_exe 1 exe ^ " in\n" ^
        Flx_srcref.short_string_of_src sr
      );
  end
  ;
*)
  match exe with
  | EXE_comment s ->
      handle_bexe (bexe_comment (sr,s)) init
  | EXE_label s ->
      state.reachable <- true;
      handle_bexe (bexe_label (sr,s)) init
  | EXE_goto s ->
      state.reachable <- false;
      handle_bexe (bexe_goto (sr,s)) init

  | EXE_cgoto e ->
    state.reachable <- false;
    let e',t as x = be e in
    if t = Flx_btype.btyp_label () then
      handle_bexe (bexe_cgoto (sr,x)) init
    else
      clierr (src_of_expr e)
      (
        "[bind_exes:ifcgoto] Computed goto require LABEL argument, got " ^
        sbt bsym_table t
      )



  | EXE_proc_return_from s ->
    state.reachable <- false;
    state.proc_return_count <- state.proc_return_count + 1;
    handle_bexe (bexe_goto (sr,"_endof_" ^ s)) init

  | EXE_ifgoto (e,s) ->
    let e',t = be e in
    if t = flx_bbool
    then
      handle_bexe (bexe_ifgoto (sr,(e',t),s)) init
    else
      clierr (src_of_expr e)
      (
        "[bind_exes:ifgoto] Conditional requires bool argument, got " ^
        sbt bsym_table t
      )

  | EXE_loop (n,e2) ->
    let be2,t2 = be e2 in
    let tbe1 =
      lookup_qn_with_sig
        state.lookup_state
        bsym_table
        sr
        sr
        state.env
        (`AST_name(sr,n,[]) : qualified_name_t)
        [t2]
    in
    (* reverse order .. *)
    let init = handle_bexe (bexe_proc_return sr) init in

    let index =
      match state.parent with
      | None -> clierr (src_of_expr e2) "no parent specified"
      | Some index -> index
    in

    (* note cal_loop actually generates a call .. *)
    handle_bexe (cal_loop state.sym_table sr tbe1 (be2,t2) index) init

  | EXE_jump (a,b) ->
    let init = bind_exe state bsym_table handle_bexe (sr, EXE_call (a, b)) init in
    let init = bind_exe state bsym_table handle_bexe (sr, EXE_proc_return) init in
    init

  | EXE_call (EXPR_name (_,"axiom_check",[]), e2) ->
    handle_bexe (bexe_axiom_check (sr,be e2)) init

  | EXE_call (f',a') ->
    begin try
    let (ea,ta) as a = be a' in
    (*
    print_endline ("Recursive descent into application " ^ string_of_expr e);
    *)
    let (bf,tf) as f  =
      match qualified_name_of_expr f' with
      | Some name ->
          let srn = src_of_qualified_name name in
          (*
          print_endline "Lookup qn with sig .. ";
          *)
          lookup_qn_with_sig
            state.lookup_state
            bsym_table
            sr
            srn
            state.env
            name
            [ta]
      | None ->
          bind_expression_with_args
            state.lookup_state
            bsym_table
            state.env
            f'
            [a]
    in
    (*
    print_endline ("tf=" ^ sbt state.sym_table tf);
    print_endline ("ta=" ^ sbt state.sym_table ta);
    *)
    begin match tf with
    | BTYP_cfunction _ ->
      handle_bexe (cal_call state bsym_table sr f a) init

    | BTYP_function _ ->
      (* print_endline "Function .. cal apply"; *)
      handle_bexe (cal_call state bsym_table sr f a) init
    | _ ->
      let apl name =
        bind_exe state bsym_table handle_bexe
          (
            sr,
            EXE_call
            (
              EXPR_name (sr, name, []),
              EXPR_tuple (sr, [f'; a'])
            )
          )
          init
      in
      apl "apply"
    end
    with x ->
      begin try 
        match f',a' with
        | EXPR_apply (sr,(f'',a'')), EXPR_tuple (_,[]) -> 
          bind_exe state bsym_table handle_bexe (sr, EXE_call (f'',a'')) init
        | _ -> raise x
      with _ -> raise x
      end
   end

  | EXE_svc s ->
    begin match lun sr s with
    | NonFunctionEntry index ->
      let index = sye index in
      let {Flx_sym.symdef=entry; id=id} =
        hfind "bexe" state.sym_table index
      in
      begin match entry with
      | SYMDEF_var _ -> ()
      | SYMDEF_val _ -> clierr sr ("Can't svc into value " ^ id)
      | SYMDEF_parameter _ -> clierr sr ("Can't svc into parameter value " ^ id)
      | _ -> clierr sr ("[bexe] svc requires variable, got " ^ id)
      end
      ;
      handle_bexe (bexe_svc (sr,index)) init

    | FunctionEntry _ -> failwith "Can't svc function!"
    end

  | EXE_proc_return ->
    state.proc_return_count <- state.proc_return_count + 1;
    state.reachable <- false;
    if do_unify state bsym_table state.ret_type (btyp_void ())
    then
      begin
        state.ret_type <- varmap_subst (Flx_lookup_state.get_varmap state.lookup_state) state.ret_type;
        handle_bexe (bexe_proc_return sr) init
      end
    else
      clierr sr
      (
        "function " ^ state.id ^ " has void return type"
      )

  | EXE_halt s ->
    state.proc_return_count <- state.proc_return_count + 1;
    state.reachable <- false;
    handle_bexe (bexe_halt (sr,s)) init

  | EXE_trace (v,s) ->
    state.proc_return_count <- state.proc_return_count + 1;
    handle_bexe (bexe_trace (sr,v,s)) init


  | EXE_fun_return e ->
    state.reachable <- false;
    state.return_count <- state.return_count + 1;
(*
print_endline ("Return expression raw " ^ string_of_expr e);
*)
    let e',t' as e = be e in
(*
print_endline ("Function return value has type " ^ sbt bsym_table t');
*)
    let t' = minimise bsym_table state.counter t' in
    ignore (do_unify state bsym_table state.ret_type t');
    state.ret_type <- varmap_subst (Flx_lookup_state.get_varmap state.lookup_state) state.ret_type;
    if type_match bsym_table state.counter state.ret_type t' then
(*
    if match maybe_matches bsym_table state.counter [state.ret_type, t'] with Some _ -> true | _ -> false then
*)
      handle_bexe (bexe_fun_return (sr,(e',t'))) init
    else if t' = BTYP_fix (0, BTYP_type 0) then begin
      print_endline "Converting return of 'any' type to procedure call";
      state.reachable <- false;
      handle_bexe (bexe_fun_return (sr,(e',state.ret_type))) init
(*
      begin match e' with
      | BEXPR_apply (f,a) -> handle_bexe (bexe_jump (sr,f,a)) init
      | _ ->
        clierr sr
          (
            "[bind_exe: fun_return ] return expression \n" ^
            sbe bsym_table e ^
            "\nof type 'any' must be application" 
          )
      end
*)
    end
    else clierr sr
      (
        "[bind_exe: fun_return ] return expression \n" ^
        sbe bsym_table e ^
        "\nof type\n" ^
        sbt bsym_table t' ^
        "\ndoes not agree with the function return type:\n" ^ 
        sbt bsym_table state.ret_type
      )

  | EXE_yield e ->
    state.return_count <- state.return_count + 1;
    let e',t' = be e in
    let t' = minimise bsym_table state.counter t' in
    ignore (do_unify state bsym_table state.ret_type t');
    state.ret_type <- varmap_subst (Flx_lookup_state.get_varmap state.lookup_state) state.ret_type;
    if type_match bsym_table state.counter state.ret_type t' then
      handle_bexe (bexe_yield (sr,(e',t'))) init
    else
      clierr sr
      (
        "In " ^ string_of_exe 0 exe ^ "\n" ^
        "Wrong return type,\nexpected : " ^
        sbt bsym_table state.ret_type ^
        "\nbut we got " ^
        sbt bsym_table t'
      )

  | EXE_nop s ->
      handle_bexe (bexe_nop (sr,s)) init

  | EXE_code s ->
      handle_bexe (bexe_code (sr,s)) init

  | EXE_noreturn_code s ->
      state.reachable <- false;
      handle_bexe (bexe_nonreturn_code (sr,s)) init

  | EXE_assert e ->
      let (x,t) as e' = be e in
      if t = flx_bbool
      then handle_bexe (bexe_assert (sr,e')) init
      else clierr sr
      (
        "assert requires bool argument, got " ^
        sbt bsym_table t
      )

  | EXE_iinit ((s,index),e) ->
(*
print_endline ("Bind EXE_iinit "^s);
*)
      let e',rhst = be e in 
      (* a type variable in executable code just has to be of kind TYPE *)
      let parent_ts = map
        (fun (s,i) -> btyp_type_var (i,btyp_type 0))
        state.parent_vs
      in
      let lhst =
        type_of_index_with_ts
          state.lookup_state
          bsym_table
          sr
          index
          parent_ts
      in
      let rhst = minimise bsym_table state.counter rhst in
      if type_match bsym_table state.counter lhst rhst
      then begin 
            let bexe = bexe_init (sr,index,(e',rhst)) in
(*
            print_endline ("Index = " ^ si index ^ " initexpr=" ^ sbe bsym_table (e',rhst) ^ " type of variable is " ^ sbt bsym_table rhst);
*)
            handle_bexe bexe init
      end else clierr sr
      (
        "[bind_exe: iinit] LHS[" ^ s ^ "<" ^ string_of_bid index ^ ">]:\n" ^
        sbt bsym_table lhst^
        "\n of initialisation must have same type as RHS:\n"^
        sbt bsym_table rhst^
        "\nunfolded LHS = " ^ sbt bsym_table (unfold lhst) ^
        "\nenvironment type variables are " ^
        print_vs state.parent_vs

      )

  | EXE_init (s,e) ->
(*
print_endline ("Bind EXE_init "^s);
*)
      begin match lun sr s with
      | FunctionEntry _ -> clierr sr "Can't init function constant"
      | NonFunctionEntry (index) ->
          let index = sye index in
          let e',rhst = be e in
          (* a type variable in executable code just has to be of kind TYPE *)
          let parent_ts = map
            (fun (s,i) -> btyp_type_var (i,btyp_type 0))
            state.parent_vs
          in
          let lhst =
            type_of_index_with_ts
              state.lookup_state
              bsym_table
              sr
              index
              parent_ts
          in
          let rhst = minimise bsym_table state.counter rhst in
          (*
          print_endline ("Checking type match " ^ sbt state.sym_table lhst ^ " ?= " ^ sbt state.sym_table rhst);
          *)
          (*
          let lhst =
            let { Flx_sym.symdef=entry; id=id} = hfind "bexe" state.sym_table index in
            match entry with
            | SYMDEF_ref _ -> btyp_pointer lhst
            | _ -> lhst
          in
          *)
          if type_match bsym_table state.counter lhst rhst
          then begin 
            let bexe = bexe_init (sr,index,(e',rhst)) in
(*
            print_endline ("Index = " ^ si index ^ " initexpr=" ^ sbe bsym_table (e',rhst) ^ " type of variable is " ^ sbt bsym_table rhst);
*)
            handle_bexe bexe init
          end else clierr sr
          (
            "[bind_exe: init] LHS[" ^ s ^ "<" ^ string_of_bid index ^ ">]:\n" ^
            sbt bsym_table lhst^
            "\n of initialisation must have same type as RHS:\n"^
            sbt bsym_table rhst^
            "\nunfolded LHS = " ^ sbt bsym_table (unfold lhst) ^
            (if length state.parent_vs > 0 then
            "\nenvironment type variables are " ^
            print_vs state.parent_vs
            else "")
          )
      end

  | EXE_assign (l,r) ->
      (* trick to generate diagnostic if l isn't an lvalue *)
      let _,lhst as lx = be l in
      let _,rhst as rx = be r in
      let lhst = minimise bsym_table state.counter lhst in
(*
print_endline ("assign: LHST = " ^ sbt bsym_table lhst);
*)
      let lhst = Flx_beta.beta_reduce "flx_bind_bexe: EXE_assign lhst" state.counter bsym_table sr lhst in
(*
print_endline ("assign after beta-reduction: LHST = " ^ sbt bsym_table lhst);
*)
      let rhst = minimise bsym_table state.counter rhst in
(*
print_endline ("assign: RHST = " ^ sbt bsym_table rhst);
*)
      let rhst = Flx_beta.beta_reduce "flx_bind_bexe: EXE_assign rhst" state.counter bsym_table sr rhst in
(*
print_endline ("assign after beta-reduction: RHST = " ^ sbt bsym_table rhst);
*)
      if type_match bsym_table state.counter lhst rhst
      then handle_bexe (bexe_assign (sr,lx,rx)) init
      else clierr sr
      (
        "[bind_exe: assign ] Assignment "^
          sbe bsym_table lx^"="^
          sbe bsym_table rx^";\n"^
        "LHS type: " ^ sbt bsym_table lhst^
        "\nmust have same type as\n"^
        "RHS type: " ^ sbt bsym_table rhst
      )

   | EXE_try -> 
     state.reachable <- true;
     handle_bexe (bexe_try sr) init

   | EXE_endtry -> 
     handle_bexe (bexe_endtry sr) init

   | EXE_catch (s,t) -> 
     state.reachable <- true;
     let t = bind_type state.lookup_state bsym_table state.env sr t in
     handle_bexe (bexe_catch sr s t) init

let bind_exes state bsym_table sr exes =
(*
  print_endline ("bind_exes.. env depth="^ string_of_int (List.length state.env));
  print_endline "Dumping Source Executables";
  print_endline "--------------------------";
  let soe e = Flx_print.string_of_expr e in
  List.iter
    (fun (_,x) -> print_endline (string_of_exe 1 x))
    exes
  ;
  print_endline ""
  ;

  print_endline "Binding Executables";
  print_endline "-------------------";
*)

  let bound_exes = List.fold_left begin fun init exe ->
    bind_exe state bsym_table (fun bexe init -> bexe :: init) exe init
  end [] exes in
  let bound_exes = List.rev bound_exes in
(*
  print_endline ""
  ;
  List.iter
    (fun x -> print_endline (string_of_bexe bsym_table 1 x))
    bound_exes
  ;
  print_endline ""
  ;
  print_endline "BINDING COMPLETE"
  ;
*)
  (* No function return statements found: it must be a procedure,
     so unify void [just a comparison with void .. heh!]
  *)
  if state.return_count = 0 then
  begin
    if 
      do_unify state bsym_table state.ret_type (btyp_void ()) || 
      (* hack, probably unify should unify it .. *)
      (match state.ret_type with BTYP_fix (0,_) -> true | _ -> false) 
    then
      state.ret_type <- varmap_subst (get_varmap state.lookup_state) state.ret_type
    else
      clierr sr
      (
        "procedure " ^ state.id ^ " has non-void return type " ^ sbt bsym_table state.ret_type
      )
  end
  ;

  begin match state.ret_type with
  | BTYP_void ->
    (*
    if
      not state.reachable &&
      state.proc_return_count = 0 (* &&
      state.syms.compiler_options.print_flag *) (* temporarily we lost the print flag *)
    then print_endline
    (
      "WARNING: procedure " ^ state.id ^
      " has no explicit return and doesn't drop thru end," ^
      "\npossible infinite loop"
    )
    *) ()

  | _ ->
    if state.reachable then begin
      (* this is now a hard error ..
         functions must manifestly return. We have to be careful
         generating code where the compiler cannot deduce
         that a final branch cannot be taken .. the user,
         however, is required to supply a dead code assertion
         to prevent the error.
      *)
(*
      print_endline "[DEBUG] Instruction sequence is:";
      List.iter begin fun exe ->
        print_endline (string_of_bexe bsym_table 0 exe)
      end bound_exes;
*)
      (*
      clierr sr
      *)
(*
      print_endline
      (
        "[bind_exes]: function " ^ state.id ^ " drops off end, missing " ^
        "return statement"
      )
*)
      (*
      ;
      print_endline "[DEBUG] Instruction sequence is:";
      List.iter begin fun exe ->
        print_endline (string_of_bexe state.sym_table 0 exe)
      end bound_exes
      *)
    end
  end
  ;
  state.ret_type, bound_exes
