open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_lookup
open Flx_mbind
open Flx_unify
open Flx_exceptions
open List
open Flx_maps

let hfind msg h k =
  try Hashtbl.find h k
  with Not_found ->
    print_endline ("flx_bexe Hashtbl.find failed " ^ msg);
    raise Not_found

(*
  THIS IS A DUMMY BOUND SYMBOL TABLE
  REQUIRED FOR THE PRINTING OF BOUND EXPRESSIONS
*)
let bbdfns = Hashtbl.create 97

let rec check_if_parent syms child parent =
  if child = parent then true
  else
      match hfind "bexe" syms.dfns child with
      | {parent=Some parent} -> check_if_parent syms child parent
      | {parent=None} -> false

let cal_call syms sr ((be1,t1) as tbe1) ((_,t2) as tbe2) =
  let be i e = bind_expression syms (build_env syms (Some i)) e in
  match unfold syms.dfns t1 with
  | `BTYP_cfunction (t, `BTYP_void)
  | `BTYP_function (t, `BTYP_void) ->
    if type_match syms.counter syms.dfns t t2
    then
      (
        (*
        match p with
        | BEXPR_closure (i,ts) ->
          begin match hfind "bexe" syms.dfns i with
          | {symdef=SYMDEF_fun _ }
          | {symdef=SYMDEF_callback _ }
            ->
            BEXE_call_prim (sr,i,ts,tbe2)

          | {symdef=SYMDEF_function _} ->
            BEXE_call_direct (sr,i,ts,tbe2)

          | _ -> assert false
          end
        | _ ->
        *)
          BEXE_call (sr,tbe1, tbe2)
      )
    else
    begin
      let reorder: tbexpr_t list option =
        match be1 with
        | BEXPR_closure (i,ts) ->
          begin match t2 with
          (* a bit of a hack .. *)
          | `BTYP_record _ | `BTYP_tuple [] ->
            let rs = match t2 with
              | `BTYP_record rs -> rs
              | `BTYP_tuple [] -> []
              | _ -> assert false
            in
            begin let pnames = match hfind "bexe" syms.dfns i with
            | {symdef=SYMDEF_function (ps,_,_,_)} ->
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
                | j,t-> BEXPR_get_n (j,tbe2),t)
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
          | _ -> BEXPR_tuple xs,`BTYP_tuple (map snd xs)
          end
        | None ->
          clierr sr
          (
            "[cal_call] Procedure " ^
            sbe syms.dfns bbdfns tbe1 ^
            "\nof type " ^
            sbt syms.dfns t1 ^
            "\napplied to argument " ^
            sbe syms.dfns bbdfns tbe2 ^
            "\n of type " ^
            sbt syms.dfns t2 ^
            "\nwhich doesn't agree with parameter type\n" ^
            sbt syms.dfns t
          )
      in BEXE_call (sr,tbe1,x2)
    end

  | _ ->
    clierr sr ("[cal_call] call non procedure, "^
    sbe syms.dfns bbdfns tbe1
    ^"\ntype=" ^ sbt syms.dfns t1)

let cal_loop syms sr ((p,pt) as tbe1) ((_,argt) as tbe2) this =
  match unfold syms.dfns pt with
  | `BTYP_function (t, `BTYP_void) ->
    if t = argt
    then
      match p with
      | BEXPR_closure (i,ts) ->
        if check_if_parent syms i this
        then
          BEXE_call (sr,(p,pt), tbe2)
          (*
          BEXE_call_direct (sr,i, ts, tbe2)
          *)
        else
          clierr sr
          "[cal_loop] Loop target must be self or parent"

      | _ ->
        clierr sr (
          "[cal_loop] Expected procedure closure, got "^
          sbe syms.dfns bbdfns (p,pt)
        )
    else
      clierr sr
      (
        "[cal_loop] Procedure " ^
        sbe syms.dfns bbdfns tbe1 ^
        "\nof type " ^
        sbt syms.dfns pt ^
        "\napplied to argument " ^
        sbe syms.dfns bbdfns tbe2 ^
        "\n of type " ^
        sbt syms.dfns argt ^
        "\nwhich doesn't agree with parameter type\n" ^
        sbt syms.dfns t
      )

  | _ ->
    clierr sr ("[cal_loop] loop to non procedure, "^
    sbe syms.dfns bbdfns (p,pt)
    ^"\ntype=" ^ string_of_btypecode syms.dfns pt)

exception Found of int

let print_vs vs =
  catmap "," (fun (s,i) -> s ^ "->" ^ si i) vs

let bind_exes syms env sr exes ret_type id index parent_vs =
  (*
  print_endline ("bind_exes.. env depth="^ string_of_int (List.length env));
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

  (* a type variable in executable code just has to be of kind TYPE *)
  let parent_ts = map (fun (s,i) -> `BTYP_var (i,`BTYP_type 0)) parent_vs in
  let ret_type = ref ret_type in
  let be e : tbexpr_t = bind_expression syms env e in
  let lun sr n = lookup_name_in_env syms env sr n in
  let luqn n = lookup_qn_in_env syms env n in
  let bt sr t : btypecode_t = bind_type syms env sr t in
  let return_count = ref 0 in
  let reachable = ref true in
  let proc_return_count = ref 0 in

  let bound_exes : bexe_t list ref = ref [] in
  let tack x = bound_exes := x :: !bound_exes in
  let rec bind_exe (sr,x) =
    (*
    print_endline ("EXE="^string_of_exe 1 x);
    *)
    if not !reachable then
    begin
      match x with
      | `EXE_label _ -> ()
      | `EXE_comment _ -> ()
      | `EXE_nop _ -> ()
      | _ -> print_endline
        (
          "WARNING: Unreachable code in "^id^": " ^
          string_of_exe 1 x ^ " in\n" ^
          Flx_srcref.short_string_of_src sr
        );
    end
    ;
    match x with
    | `EXE_comment s ->       tack (BEXE_comment (sr,s))
    | `EXE_label s ->         reachable := true; tack (BEXE_label (sr,s))
    | `EXE_goto s ->          reachable := false; tack (BEXE_goto (sr,s))

    | `EXE_ifgoto (e,s) ->
      let e',t = be e in
      if t = flx_bbool
      then tack (BEXE_ifgoto (sr,(e',t), s))
      else
        clierr (src_of_expr e)
        (
          "[bind_exes:ifgoto] Conditional requires bool argument, got " ^
          string_of_btypecode syms.dfns t
        )

    | `EXE_loop (n,e2) ->
      let be2,t2 = be e2 in
      let tbe1 =
         lookup_qn_with_sig
         syms
         sr sr
         env
         (`AST_name(sr,n,[]) : qualified_name_t)
         [t2]
      in
        (* reverse order .. *)
        tack (BEXE_proc_return sr);
        (* note cal_loop actually generates a call .. *)
        tack (cal_loop syms sr tbe1 (be2,t2) index)

    | `EXE_jump (a,b) ->
      bind_exe (sr,`EXE_call (a,b));
      bind_exe  (sr,`EXE_proc_return)

    | `EXE_call (`AST_name (_,"axiom_check",[]), e2) ->
       tack (BEXE_axiom_check(sr,be e2))

    | `EXE_call (f',a') ->
      (*
      print_endline ("Apply " ^ string_of_expr f' ^ " to " ^  string_of_expr a');
      *)
      let (ea,ta) as a = be a' in
      (*
      print_endline ("Recursive descent into application " ^ string_of_expr e);
      *)
      let (bf,tf) as f  =
        match f' with
        | #qualified_name_t as name ->
          let srn = src_of_expr name in
          (*
          print_endline "Lookup qn with sig .. ";
          *)
          lookup_qn_with_sig syms sr srn env name [ta]
        | _ -> bind_expression_with_args syms env f' [a]
      in
      (*
      print_endline ("tf=" ^ sbt syms.dfns tf);
      print_endline ("ta=" ^ sbt syms.dfns ta);
      *)
      begin match tf with
      | `BTYP_cfunction _ ->
        tack (cal_call syms sr f a)

      | `BTYP_function _ ->
        (* print_endline "Function .. cal apply"; *)
        tack (cal_call syms sr f a)
      | _ ->
        let apl name =
          bind_exe
          (
            sr,
            `EXE_call
            (
              `AST_name (sr,name,[]),
              `AST_tuple (sr,[f';a'])
            )
          )
        in
        apl "apply"
      end

(*

    | `EXE_call (f', a') -> (* OVERLOADING *)
      let sr = src_of_expr sn in
      let be2,t2 = be e2 in
      let (be1,t1) as tbe1 =
         match sn with
         | #qualified_name_t as qn ->
           lookup_qn_with_sig
           syms
           sr sr
           env
           qn [t2]
         | _ -> be sn
      in
        tack (cal_call syms sr tbe1 (be2,t2))

    | `EXE_call (p,e) ->
      let p',pt' = be p and e',et' = be e in
      tack (cal_call syms sr (p', pt') (e', et'))
*)

    | `EXE_svc s ->
      begin match lun sr s with
      | NonFunctionEntry (index) ->
        let index = sye index in
        let {symdef=entry; id=id} = hfind "bexe" syms.dfns index in
        begin match entry with
        | SYMDEF_var _ -> ()
        | SYMDEF_val _ -> clierr sr ("Can't svc into value " ^ id)
        | SYMDEF_parameter _ -> clierr sr ("Can't svc into parameter value " ^ id)
        | _ -> clierr sr ("[bexe] svc requires variable, got " ^ id)
        end
        ;
        tack (BEXE_svc (sr,index))

      | FunctionEntry _ -> failwith "Can't svc function!"
      end

    | `EXE_proc_return ->
      incr proc_return_count;
      reachable := false;
      if do_unify syms !ret_type `BTYP_void
      then
        begin
          ret_type := varmap_subst syms.varmap !ret_type;
          tack (BEXE_proc_return sr)
        end
      else
        clierr sr
        (
          "function " ^id^" has void return type"
        )

    | `EXE_halt s ->
      incr proc_return_count;
      reachable := false;
      tack (BEXE_halt (sr,s))

    | `EXE_trace (v,s) ->
      incr proc_return_count;
      tack (BEXE_trace (sr,v,s))


    | `EXE_fun_return e ->
      reachable := false;
      incr return_count;
      let e',t' as e = be e in
      let t' = minimise syms.counter syms.dfns t' in
      ignore(do_unify syms !ret_type t');
      ret_type := varmap_subst syms.varmap !ret_type;
      if type_match syms.counter syms.dfns !ret_type t' then
        tack (BEXE_fun_return (sr,(e',t')))
      else clierr sr
        (
          "[bind_exe: fun_return ] return of  "^sbe syms.dfns bbdfns e ^":\n"^
          "fun return type:\n" ^ string_of_btypecode syms.dfns !ret_type^
          "\nmust have same type as return expression:\n"^
          string_of_btypecode syms.dfns t'
        )

    | `EXE_yield e ->
      incr return_count;
      let e',t' = be e in
      let t' = minimise syms.counter syms.dfns t' in
      ignore(do_unify syms !ret_type t');
      ret_type := varmap_subst syms.varmap !ret_type;
      if type_match syms.counter syms.dfns !ret_type t' then
        tack (BEXE_yield (sr,(e',t')))
      else
        clierr sr
        (
          "In " ^ string_of_exe 0 x ^ "\n" ^
          "Wrong return type,\nexpected : " ^
          string_of_btypecode syms.dfns !ret_type ^
          "\nbut we got " ^
          string_of_btypecode syms.dfns t'
        )

    | `EXE_nop s ->           tack (BEXE_nop (sr,s))
    | `EXE_code s ->          tack (BEXE_code (sr,s))
    | `EXE_noreturn_code s ->
      reachable := false;
      tack (BEXE_nonreturn_code (sr,s))

    | `EXE_assert e ->
      let (x,t) as e' = be e in
      if t = flx_bbool
      then tack (BEXE_assert (sr,e'))
      else clierr sr
      (
        "assert requires bool argument, got " ^
        string_of_btypecode syms.dfns t
      )

    | `EXE_iinit ((s,index),e) ->
        let e',rhst = be e in
        let lhst = type_of_index_with_ts syms sr index parent_ts in
        let rhst = minimise syms.counter syms.dfns rhst in
        let lhst = reduce_type lhst in
        if type_match syms.counter syms.dfns lhst rhst
        then tack (BEXE_init (sr,index, (e',rhst)))
        else clierr sr
        (
          "[bind_exe: iinit] LHS["^s^"<"^si index^">]:\n"^
          string_of_btypecode syms.dfns lhst^
          "\n of initialisation must have same type as RHS:\n"^
          string_of_btypecode syms.dfns rhst^
          "\nunfolded LHS = " ^ sbt syms.dfns (unfold syms.dfns lhst) ^
          "\nenvironment type variables are " ^
          print_vs parent_vs

        )

    | `EXE_init (s,e) ->
      begin match lun sr s with
      | FunctionEntry _ -> clierr sr "Can't init function constant"
      | NonFunctionEntry (index) ->
        let index = sye index in
        let e',rhst = be e in
        let lhst = type_of_index_with_ts syms sr index parent_ts in
        let rhst = minimise syms.counter syms.dfns rhst in
        let lhst = reduce_type lhst in
        (*
        print_endline ("Checking type match " ^ sbt syms.dfns lhst ^ " ?= " ^ sbt syms.dfns rhst);
        *)
        (*
        let lhst =
          let {symdef=entry; id=id} = hfind "bexe" syms.dfns index in
          match entry with
          | SYMDEF_ref _ -> `BTYP_pointer lhst
          | _ -> lhst
        in
        *)
        if type_match syms.counter syms.dfns lhst rhst
        then tack (BEXE_init (sr,index, (e',rhst)))
        else clierr sr
        (
          "[bind_exe: init] LHS["^s^"<"^si index^">]:\n"^
          string_of_btypecode syms.dfns lhst^
          "\n of initialisation must have same type as RHS:\n"^
          string_of_btypecode syms.dfns rhst^
          "\nunfolded LHS = " ^ sbt syms.dfns (unfold syms.dfns lhst) ^
          (if length parent_vs > 0 then
          "\nenvironment type variables are " ^
          print_vs parent_vs
          else "")
        )
      end

    | `EXE_assign (l,r) ->
      let _,lhst as lx = be l in
      let _,rhst as rx = be r in
      let lhst = reduce_type lhst in
      let rhst = reduce_type rhst in
      let lhst = minimise syms.counter syms.dfns lhst in
      let rhst = minimise syms.counter syms.dfns rhst in
      if type_match syms.counter syms.dfns lhst rhst
      then tack (BEXE_assign (sr,lx, rx))
      else clierr sr
      (
        "[bind_exe: assign ] Assignment "^
          sbe syms.dfns bbdfns lx^"="^
          sbe syms.dfns bbdfns rx^";\n"^
        "LHS type: " ^ string_of_btypecode syms.dfns lhst^
        "\nmust have same type as\n"^
        "RHS type: " ^ string_of_btypecode syms.dfns rhst
      )
  in
  List.iter bind_exe exes;
  let bound_exes = List.rev !bound_exes in
  (*
  print_endline ""
  ;
  List.iter
    (fun x -> print_endline (string_of_bexe syms.dfns 1 x))
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
  if !return_count = 0 then
  begin
    if do_unify syms !ret_type `BTYP_void
    then
      ret_type := varmap_subst syms.varmap !ret_type
    else
      clierr sr
      (
        "procedure " ^id^" has non-void return type"
      )
  end
  ;

  begin match !ret_type with
  | `BTYP_void ->
    if
      not !reachable &&
      !proc_return_count = 0 &&
      syms.compiler_options.print_flag
    then print_endline
    (
      "WARNING: procedure " ^id^
      " has no explicit return and doesn't drop thru end," ^
      "\npossible infinite loop"
    )
  | _ ->
    if !reachable then begin
      (* this is now a hard error ..
         functions must manifestly return. We have to be careful
         generating code where the compiler cannot deduce
         that a final branch cannot be taken .. the user,
         however, is required to supply a dead code assertion
         to prevent the error.
      *)
      print_endline "[DEBUG] Instruction sequence is:";
      iter (fun exe -> print_endline (string_of_bexe syms.dfns bbdfns 0 exe)) bound_exes;
      clierr sr
      (
        "[bind_exes]: function "^id^" drops off end, missing return statement"
      )
      (*
      ;
      print_endline "[DEBUG] Instruction sequence is:";
      iter (fun exe -> print_endline (string_of_bexe syms.dfns 0 exe)) bound_exes
      *)
    end
  end
  ;
  !ret_type,bound_exes
