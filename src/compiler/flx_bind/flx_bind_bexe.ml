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
open Flx_unify
open Flx_exceptions
open List
open Flx_maps
open Flx_lookup_state
open Flx_bexe_state

type pin_descr_t = string * (string * int * Flx_btype.t)
type device_descr_t = string * pin_descr_t list

let typecheck_fields bsym_table name l r =
  let counter = ref 1 in (* hack, but should be enough *)
  if type_eq bsym_table counter l r then "" else
  "\nField " ^ name ^ 
   " LHS type " ^ sbt bsym_table l ^
   " not same as RHS type " ^ 
   sbt bsym_table r

let field_list_diag bsym_table lhs rhs =
  let lcheck = 
    List.fold_left (fun acc (name,typ) -> 
      if List.mem_assoc name rhs then acc ^ typecheck_fields bsym_table name typ (List.assoc name rhs) else
       acc ^"\nLHS field " ^ name ^ " not present in RHS"
    ) "" lhs
  in
  let rcheck = 
    List.fold_left (fun acc (name,typ) -> 
      if List.mem_assoc name lhs then acc else
       acc ^"\nRHS field " ^ name ^ " not present in LHS"
    ) "" rhs
  in
  lcheck ^ rcheck


let record_field_diag bsym_table lhst rhst = 
  match lhst, rhst with
  | BTYP_record lhs, BTYP_record rhs -> field_list_diag bsym_table lhs rhs
  | _,_ -> ""


let do_unify state bsym_table a b =
  Flx_do_unify.do_unify state.counter (Flx_lookup_state.get_varmap state.lookup_state)  state.sym_table bsym_table a b

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
print_endline "Cal call, types don't match ..";
      let reorder =
        match be1 with
        | BEXPR_closure (i,ts) ->
          begin match t2 with
          (* a bit of a hack .. *)
          | BTYP_record _ | BTYP_tuple [] ->
            let rs = match t2 with
              | BTYP_record (rs) -> rs
              | BTYP_tuple [] -> []
              | _ -> assert false
            in
            begin let pnames = match hfind "bexe" state.sym_table i with
            | { Flx_sym.symdef=SYMDEF_function (ps,_,_,_,_)} ->
              map (fun (_,name,_,d)->
                name,
                match d with None -> None | Some e -> Some (be i e)
              ) (fst ps)
            | _ -> assert false
            in
            let n = length rs in
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
          clierrx "[flx_bind/flx_bind_bexe.ml:157: E9] " sr
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
  match unfold "flx_bind_bexe" t1 with

  (* special handling of non-returning function. Instead of returning
     void we return type any, 
  *)
  | BTYP_cfunction (t, BTYP_fix (0,_))
  | BTYP_function (t, BTYP_fix (0,_)) 
  | BTYP_effector (t, _, BTYP_fix (0,_)) 
    ->
    let a = genargs t in
    bexe_jump a

  | BTYP_cfunction (t, BTYP_void)
  | BTYP_function (t, BTYP_void) 
  | BTYP_effector (t, _, BTYP_void) 
    ->
    let a = genargs t in
    bexe_call a

  | _ ->
    clierrx "[flx_bind/flx_bind_bexe.ml:193: E10] " sr ("[cal_call] call non procedure, "^
    sbe bsym_table tbe1
    ^"\ntype=" ^ sbt bsym_table t1 
    ^ "\nPerhaps you meant to assign this expression to something. ")

let cal_loop sym_table sr ((p,pt) as tbe1) ((_,argt) as tbe2) this =
  match unfold "flx_bind_bexe" pt with
  | BTYP_function (t, BTYP_void)
  | BTYP_effector (t, _, BTYP_void) ->
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
          clierrx "[flx_bind/flx_bind_bexe.ml:212: E11] " sr
          "[cal_loop] Loop target must be self or parent"

      | _ ->
        clierrx "[flx_bind/flx_bind_bexe.ml:216: E12] " sr (
          "[cal_loop] Expected procedure closure, got "^
          sbe bsym_table (p,pt)
        )
    else
      clierrx "[flx_bind/flx_bind_bexe.ml:221: E13] " sr
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
    clierrx "[flx_bind/flx_bind_bexe.ml:236: E14] " sr ("[cal_loop] loop to non procedure, "^
    sbe bsym_table (p,pt)
    ^"\ntype=" ^ sbt bsym_table pt)

exception Found of int

let print_vs vs =
  catmap "," (fun (s,i) -> s ^ "->" ^ string_of_bid i) vs

type bexe_t = Flx_bexe.t

let rec bind_exe' (state: Flx_bexe_state.bexe_state_t) bsym_table (sr, exe) : bexe_t list =
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
print_endline ("Bind_exe, return type " ^ Flx_print.sbt bsym_table state.ret_type);
  print_endline ("EXE="^string_of_exe 1 exe);
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
  | EXE_begin_match_case
  | EXE_end_match_case -> assert false
  | EXE_circuit cs -> Flx_bind_circuit.bind_circuit bsym_table state sr be cs 

  | EXE_comment s ->
      [bexe_comment (sr,s)]

  | EXE_type_error x ->
    let result = try Some (bind_exe' state bsym_table (sr, x)) with _ -> None
    in begin match result with
    | None -> []
    | Some _ -> clierrx "[flx_bind/flx_bind_bexe.ml:295: E15] " sr ("type_error expected, statement compiled! " ^ string_of_exe 0 exe);
    end

  | EXE_label s ->
      state.reachable <- true;
      let maybe_index = lookup_label_in_env state.lookup_state bsym_table state.env sr s in
      begin match maybe_index with
      | None -> clierrx "[flx_bind/flx_bind_bexe.ml:302: E16] " sr ("[bind_exe:EXE_label] Can't find label " ^ s)
      | Some i -> [bexe_label (sr,i)] 
      end

  | EXE_goto s ->
      state.reachable <- false;
      let maybe_index = lookup_label_in_env state.lookup_state bsym_table state.env sr s in
      begin match maybe_index with
      | None -> clierrx "[flx_bind/flx_bind_bexe.ml:310: E17] " sr ("bind_exe:EXE_goto] Can't find label " ^ s)
      | Some i -> [bexe_goto (sr,i)] 
      end

  | EXE_cgoto e ->
    state.reachable <- false;
    let e',t as x = be e in
    if t = Flx_btype.btyp_label () then
      begin match e' with
      | BEXPR_label (i) -> 
        (*
        print_endline ("optimised cgoto to goto: label " ^ s);
        *)
        [bexe_goto (sr,i)]
      | _ -> [(bexe_cgoto (sr,x))]
      end
    else
      clierrx "[flx_bind/flx_bind_bexe.ml:327: E18] " (src_of_expr e)
      (
        "[bind_exes:ifcgoto] Computed goto require LABEL argument, got " ^
        sbt bsym_table t
      )

  | EXE_ifcgoto (e1,e2) ->
    state.reachable <- false;
    let e1',t1 as x1 = be e1 in
    if t1 = flx_bbool
    then
      let e2',t2 as x2 = be e2 in
      if t2 = Flx_btype.btyp_label () then
        begin match e2' with
        | BEXPR_label (i) -> 
          (*
          print_endline ("optimised cgoto to goto: label " ^ s);
          *)
          [bexe_ifgoto (sr,x1,i)]
        | _ -> [(bexe_ifcgoto (sr,x1,x2))]
        end
      else
        clierrx "[flx_bind/flx_bind_bexe.ml:349: E19] " (src_of_expr e2)
        (
          "[bind_exes:ifcgoto] Computed goto require LABEL argument, got " ^
          sbt bsym_table t2
        )
    else
      clierrx "[flx_bind/flx_bind_bexe.ml:355: E20] " (src_of_expr e1)
      (
        "[bind_exes:ifgoto] Conditional requires bool argument, got " ^
        sbt bsym_table t1
      )

  | EXE_proc_return_from s ->
    state.reachable <- false;
    state.proc_return_count <- state.proc_return_count + 1;
    let label_name = "_endof_" ^ s in
    let maybe_index = lookup_label_in_env state.lookup_state bsym_table state.env sr label_name in
    begin match maybe_index with
    | None -> clierrx "[flx_bind/flx_bind_bexe.ml:367: E21] " sr ("[bind_exe:EXE_proc_return_from] Can't find label " ^ s)
    | Some i -> [bexe_goto (sr,i)] 
    end

  | EXE_ifgoto (e,s) ->
    let e',t = be e in
    if t = flx_bbool
    then
      let maybe_index = lookup_label_in_env state.lookup_state bsym_table state.env sr s in
      begin match maybe_index with
      | None -> clierrx "[flx_bind/flx_bind_bexe.ml:377: E22] " sr ("[bind_exe:EXE_if_goto] Can't find label " ^ s)
      | Some i -> [bexe_ifgoto (sr,(e',t),i)] 
      end
    else
      clierrx "[flx_bind/flx_bind_bexe.ml:381: E23] " (src_of_expr e)
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
    let init = [(bexe_proc_return sr)] in

    let index =
      match state.parent with
      | None -> clierrx "[flx_bind/flx_bind_bexe.ml:404: E24] " (src_of_expr e2) "no parent specified"
      | Some index -> index
    in

    (* note cal_loop actually generates a call .. *)
    [(cal_loop state.sym_table sr tbe1 (be2,t2) index)]

  | EXE_jump (a,b) ->
    bind_exe' state bsym_table (sr, EXE_call (a, b)) @
    bind_exe' state bsym_table (sr, EXE_proc_return) 

  | EXE_call (EXPR_name (_,"axiom_check",[]), e2) ->
    [(bexe_axiom_check (sr,be e2))]

  | EXE_call (EXPR_apply(_,(EXPR_name (_,"_iter",[]), EXPR_name (_,fn,[]))),arg) ->
    Flx_gmap.generic_map_proc bsym_table (bind_exe' state) be sr fn arg 

  | EXE_call (EXPR_apply(_,(EXPR_name (_,"_rev_iter",[]), EXPR_name (_,fn,[]))),arg) ->
    Flx_gmap.generic_rev_map_proc bsym_table (bind_exe' state) be sr fn arg 



  (* do overloading here FIXME ?? *)
  | EXE_call_with_trap (f',a') ->
    let (ea,ta) as a = be a' in
    let (ef,tf) as f = be f' in
    begin match tf with
    | BTYP_function (d,BTYP_void) ->
      if type_eq bsym_table state.counter d ta then
        [bexe_call_with_trap (sr,f,a)]
      else 
        clierr sr ("call with trap requires procedure domain and argument to be the same type\n"^
        "got domain        " ^ sbt bsym_table d ^ "\n" ^
        "got argument type " ^ sbt bsym_table ta)

   | _ ->  clierr sr ("call with trap requires first argument to be procedure, got " ^ string_of_expr a')
   end

  | EXE_call (f',a') ->
(*
print_endline ("Binding call f=" ^ string_of_expr f' ^ ",a=" ^ string_of_expr a');
*)
    begin try
    let (ea,ta) as a = be a' in
(*
print_endline ("        >>> Call, bound argument is type " ^ sbt bsym_table ta);
*)
    (*
    print_endline ("Recursive descent into application " ^ string_of_expr e);
    *)
    let (bf,tf) as f  =
      match qualified_name_of_expr f' with
      | Some name ->
          let srn = src_of_qualified_name name in
          (*
          print_endline "Lookup using qn with sig .. ";
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
          (*
          print_endline "Lookup using bind_expression with args .. ";
          *)
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
    | BTYP_cfunction _
    | BTYP_function _ 
    | BTYP_effector _ ->
      begin try
      [(cal_call state bsym_table sr f a)]
      with exn ->
      try
      let apl name =
        bind_exe' state bsym_table 
          (
            sr,
            EXE_call
            (
              EXPR_name (sr, name, []),
              EXPR_tuple (sr, [f'; a'])
            )
          )
      in
      apl "apply"
      with _ -> raise exn (* raise original error *)
      end
    | _ ->
      let apl name =
        bind_exe' state bsym_table 
          (
            sr,
            EXE_call
            (
              EXPR_name (sr, name, []),
              EXPR_tuple (sr, [f'; a'])
            )
          )
      in
      apl "apply"
    end
    with x ->
      begin try 
        match f',a' with
        | EXPR_apply (sr,(f'',a'')), EXPR_tuple (_,[]) -> 
          bind_exe' state bsym_table (sr, EXE_call (f'',a''))
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
      | SYMDEF_val _ -> clierrx "[flx_bind/flx_bind_bexe.ml:520: E25] " sr ("Can't svc into value " ^ id)
      | SYMDEF_parameter _ -> clierrx "[flx_bind/flx_bind_bexe.ml:521: E26] " sr ("Can't svc into parameter value " ^ id)
      | _ -> clierrx "[flx_bind/flx_bind_bexe.ml:522: E27] " sr ("[bexe] svc requires variable, got " ^ id)
      end
      ;
      [(bexe_svc (sr,index))]

    | FunctionEntry _ -> failwith "Can't svc function!"
    end

  | EXE_proc_return ->
    state.proc_return_count <- state.proc_return_count + 1;
    state.reachable <- false;
    if do_unify state bsym_table state.ret_type (btyp_void ())
    then
      begin
        state.ret_type <- varmap_subst (Flx_lookup_state.get_varmap state.lookup_state) state.ret_type;
        [(bexe_proc_return sr)]
      end
    else
      clierrx "[flx_bind/flx_bind_bexe.ml:540: E28] " sr
      (
        "function " ^ state.id ^ " has void return type"
      )

  | EXE_halt s ->
    state.proc_return_count <- state.proc_return_count + 1;
    state.reachable <- false;
    [(bexe_halt (sr,s))]

  | EXE_trace (v,s) ->
    state.proc_return_count <- state.proc_return_count + 1;
    [(bexe_trace (sr,v,s))]


  | EXE_fun_return e ->
    state.reachable <- false;
    state.return_count <- state.return_count + 1;
(*
print_endline ("++++++++ EXE_fun_return1: Return expression raw " ^ string_of_expr e);
*)
    let e',t' as e = be e in
(*
print_endline ("+++++++++EXE_fun_return2: Function return value has type " ^ sbt bsym_table t');
*)
    let t' = minimise bsym_table state.counter t' in
(*
print_endline ("Function return value has MINIMISED type " ^ sbt bsym_table t');
*)
    ignore (do_unify state bsym_table state.ret_type t');
    state.ret_type <- varmap_subst (Flx_lookup_state.get_varmap state.lookup_state) state.ret_type;
    if type_match bsym_table state.counter state.ret_type t' then
(*
    if match maybe_matches bsym_table state.counter [state.ret_type, t'] with Some _ -> true | _ -> false then
*)
      [(bexe_fun_return (sr,(e',t')))]
    else if t' = btyp_fix 0 (btyp_type 0) then begin
      print_endline "Converting return of 'any' type to procedure call";
      state.reachable <- false;
      [(bexe_fun_return (sr,(e',state.ret_type)))]
(*
      begin match e' with
      | BEXPR_apply (f,a) -> [(bexe_jump (sr,f,a))]
      | _ ->
        clierrx "[flx_bind/flx_bind_bexe.ml:584: E29] " sr
          (
            "[bind_exe: fun_return ] return expression \n" ^
            sbe bsym_table e ^
            "\nof type 'any' must be application" 
          )
      end
*)
    end
    else clierrx "[flx_bind/flx_bind_bexe.ml:593: E30] " sr
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
      [(bexe_yield (sr,(e',t')))]
    else
      clierrx "[flx_bind/flx_bind_bexe.ml:612: E31] " sr
      (
        "In " ^ string_of_exe 0 exe ^ "\n" ^
        "Wrong return type,\nexpected : " ^
        sbt bsym_table state.ret_type ^
        "\nbut we got " ^
        sbt bsym_table t'
      )

  | EXE_nop (s) ->
      [bexe_nop (sr,s)]

  | EXE_code (s,e) ->
      [bexe_code (sr,s,be e)]

  | EXE_noreturn_code (s,e) ->
      state.reachable <- false;
      [bexe_nonreturn_code (sr,s,be e)]

  | EXE_assert e ->
      let (x,t) as e' = be e in
      if t = flx_bbool
      then [(bexe_assert (sr,e'))]
      else clierrx "[flx_bind/flx_bind_bexe.ml:635: E32] " sr
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
      let lhst = Flx_unify.normalise_tuple_cons bsym_table lhst in
      let rhst = minimise bsym_table state.counter rhst in
      let rhst = Flx_unify.normalise_tuple_cons bsym_table rhst in
      if type_match bsym_table state.counter lhst rhst
      then begin 
            let bexe = bexe_init (sr,index,(e',rhst)) in
(*
            print_endline ("Index = " ^ si index ^ " initexpr=" ^ sbe bsym_table (e',rhst) ^ " type of variable is " ^ sbt bsym_table rhst);
*)
            [bexe]
      end else clierrx "[flx_bind/flx_bind_bexe.ml:669: E33] " sr
      (
        "[bind_exe: iinit] LHS[" ^ s ^ "<" ^ string_of_bid index ^ ">]:\n" ^
        sbt bsym_table lhst^
        "\n of initialisation must have same type as RHS:\n"^
        sbt bsym_table rhst^
        "\nunfolded LHS = " ^ sbt bsym_table (unfold "flx_bind_bexe" lhst) ^
        "\nenvironment type variables are " ^
        print_vs state.parent_vs

      )

  | EXE_init (s,e) ->
(*
print_endline ("Bind EXE_init "^s);
*)
      begin match lun sr s with
      | FunctionEntry _ -> clierrx "[flx_bind/flx_bind_bexe.ml:686: E34] " sr "Can't init function constant"
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
          let lhst = Flx_unify.normalise_tuple_cons bsym_table lhst in
          let rhst = minimise bsym_table state.counter rhst in
          let rhst = Flx_unify.normalise_tuple_cons bsym_table rhst in
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
            [bexe]
          end else clierrx "[flx_bind/flx_bind_bexe.ml:724: E35] " sr
          (
            "[bind_exe: init] LHS[" ^ s ^ "<" ^ string_of_bid index ^ ">]:\n" ^
            sbt bsym_table lhst^
            "\n of initialisation must have same type as RHS:\n"^
            sbt bsym_table rhst^
            "\nunfolded LHS = " ^ sbt bsym_table (unfold "flx_bind_bexe" lhst) ^
            (if length state.parent_vs > 0 then
            "\nenvironment type variables are " ^
            print_vs state.parent_vs
            else "")
          )
      end

  | EXE_assign (l,r) ->
(*
print_endline ("BINDING ASSIGNMENT " ^ string_of_exe 0 exe);
*)
      (* trick to generate diagnostic if l isn't an lvalue *)
      let _,lhst as lx = be l in
      let _,rhst as rx = be r in
(*
print_endline ("assign: LHS=" ^ sbe bsym_table lx ^ ", LHST = " ^ sbt bsym_table lhst);
*)
      let lhst = minimise bsym_table state.counter lhst in
      let lhst = Flx_beta.beta_reduce "flx_bind_bexe: EXE_assign lhst" state.counter bsym_table sr lhst in
      let lhst = Flx_unify.normalise_tuple_cons bsym_table lhst in
(*
print_endline ("assign after beta-reduction: LHST = " ^ sbt bsym_table lhst);
*)
(*
print_endline ("assign:  RHS=" ^ sbe bsym_table rx ^ ",RHST = " ^ sbt bsym_table rhst);
*)
      let rhst = minimise bsym_table state.counter rhst in
      let rhst = Flx_beta.beta_reduce "flx_bind_bexe: EXE_assign rhst" state.counter bsym_table sr rhst in
      let rhst = Flx_unify.normalise_tuple_cons bsym_table rhst in
(*
print_endline ("assign after beta-reduction: RHST = " ^ sbt bsym_table rhst);
*)
      if type_match bsym_table state.counter lhst rhst
      then [(bexe_assign (sr,lx,rx))]
      else clierrx "[flx_bind/flx_bind_bexe.ml:765: E36] " sr
      (
        "[bind_exe: assign ] Assignment "^
          sbe bsym_table lx^"="^
          sbe bsym_table rx^";\n"^
        "LHS type: " ^ sbt bsym_table lhst^
        "\nmust have same type as\n"^
        "RHS type: " ^ sbt bsym_table rhst ^
        record_field_diag bsym_table lhst rhst
      )

   | EXE_try -> 
     state.reachable <- true;
     [(bexe_try sr)]

   | EXE_endtry -> 
     [(bexe_endtry sr)]

   | EXE_catch (s,t) -> 
     state.reachable <- true;
     let t = bind_type state.lookup_state bsym_table state.env sr t in
     [(bexe_catch sr s t)]

let rec bind_exe (state: Flx_bexe_state.bexe_state_t) bsym_table (sr, exe) : bexe_t list =
  try bind_exe' state bsym_table (sr,exe)
  with Flx_dot.OverloadResolutionError as exn ->
     print_endline  ("Overload resolution error binding exe: " ^ string_of_exe 2 exe);
     print_endline (Flx_srcref.long_string_of_src sr);
     raise exn

let bind_exes state bsym_table sr exes : Flx_btype.t * Flx_bexe.t list  =
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

  let rec get_subextent (exes : sexe_t list) : bexe_t list * sexe_t list =
    let rec bind (exes: sexe_t list) (result : bexe_t list) : bexe_t list * sexe_t list  =
      match exes with
      | [] -> result, []
      | (_,EXE_end_match_case) :: tail -> result, tail
      | (_,EXE_begin_match_case) :: tail ->
        begin try
          let processed, tail = get_subextent tail in
          bind tail (result @ processed) 
        with GadtUnificationFailure ->
          (* skip up to end of match case, take account of 
             any nested match cases too, this should drop
             the WHOLE subextent containing a unification
             failure
          *)
          let rec skip (ls: sexe_t list): sexe_t list = 
            match ls with 
            | [] -> assert false
            | (_,EXE_end_match_case) :: tail -> tail
            | (_,EXE_begin_match_case) :: tail -> 
              skip (skip tail)
            | h :: tail -> skip tail
          in
          bind (skip tail) result
        end

      | exe :: tail -> 
        let bs =  (bind_exe state bsym_table exe) in
        bind tail (result @ bs)
    in
    bind exes [] 
  in
  let bound_exes, tail = get_subextent exes in
  assert (List.length tail = 0);
(*
  let bound_exes = List.fold_left (fun acc exe ->
    try
      let result = bind_exe state bsym_table  exe in
      List.rev result @ acc
    with GadtUnificationFailure ->
print_endline ("GADT UNIFICATION FAILURE generating exe " ^ string_of_exe 2 (snd exe));
      acc
  )
  [] exes 
  in
  let bound_exes = List.rev bound_exes in
*)
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
      clierrx "[flx_bind/flx_bind_bexe.ml:836: E37] " sr
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
      clierrx "[flx_bind/flx_bind_bexe.ml:874: E38] " sr
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


