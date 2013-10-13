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

(*
  Order of sugars for operator dot.

  1. If the lhs and rhs are functions and the codomain of the lhs agrees with
      the domain of the rhs, this is a reverse composition, eg:

      1 . (str . trim) --> compose(trim, str) 1
 
  2. If the lhs is a struct, cstruct, or record AND the rhs is a simple name
      AND the name is a component, it's the component

  3. Try get method (now, for any type not just struct!)

  4. Try reverse application

  5. Deref once, try for field name. If found, result is POINTER to component.

  PROPERTIES
  ----------

  Commutativity of reverse composition and reverse application, eg:

  1 . str . trim == (1 . str) . trim == 1 . (str . trim)

  This property is VITAL for comprehension: the dot operator is syntactically
  associative. SPECIAL NOTE: COUNTEREXAMPLE: for any non-polymorphic function
  it is impossible to confuse composition and application. But for polymorphic
  functions it's possible! Therefore the order MATTERS.
  

*)

exception Not_field

let handle_field_name state bsym_table build_env env rs be bt koenig_lookup cal_apply bind_type' mkenv 
  sr e e2 name ts i ts' isptr
=
  let rt t = beta_reduce "flx_dot: handle_field_name" state.counter bsym_table sr t in
  let (_,t) as te = be e in
  let ttt =rt t in
  match hfind "lookup" state.sym_table i with

  (* STRUCT *)
  | { Flx_sym.id=id; sr=sra; symdef=SYMDEF_struct ls }
  | { Flx_sym.id=id; sr=sra; symdef=SYMDEF_cstruct (ls,_) } ->
    let _,vs,_ = find_split_vs state.sym_table bsym_table i in
    let cidx,ct =
      let rec scan i = function
      | [] -> raise Not_field
      | (vn,vat)::_ when vn = name -> i,vat
      | _:: t -> scan (i+1) t
      in scan 0 ls
    in
    let ct =
      let bvs = List.map
        (fun (n,i,_) -> n, btyp_type_var (i, btyp_type 0))
        (vs)
      in
      let env' = build_env state bsym_table (Some i) in
      bind_type' state bsym_table env' rsground sr ct bvs mkenv
    in
    let vs' = List.map (fun (s,i,tp) -> s,i) vs in
    let ct = tsubst vs' ts' ct in
    let ct = if isptr then btyp_pointer ct else ct in
    (* messy .. we generalised get_n to accept any type instead
       of an integer selector. to replace integer n,
       we have to use case n of m where m is the number of
       cases: n is an int, whereas m is unitsum m' where m'
       is the number of cases.

       Note: bexpr_case is bugged! It can only be used
       for const constructors, the type of the case of T
       is always T. 
    *)
    bexpr_get_n ct cidx te
  | _ ->  raise Not_field

let handle_dot state bsym_table build_env env rs be bt koenig_lookup cal_apply bind_type' sr e e2 = assert false;
  let mkenv i = build_env state bsym_table (Some i) in
  let rt t = beta_reduce "flx_dot: handle-dot" state.counter bsym_table sr t in

  let (_,tt) as te = be e in (* polymorphic! *)
  let ttt = rt tt in
  begin match e2 with

  (* RHS IS A SIMPLE NAME *)
  | EXPR_name (_,name,ts) ->
    begin match ttt with

    (* LHS HAS POINTER TYPE *)
    | BTYP_pointer (BTYP_inst (i,ts')) ->
      begin 
      try
        handle_field_name state bsym_table build_env env rs be bt 
          koenig_lookup cal_apply bind_type' mkenv sr e e2 name ts i ts' true
      with Not_field ->
      try be (EXPR_apply (sr,(e2,e)))
      with exn ->
        clierr sr
        (
          "[bind_expression] operator dot: Field " ^ name ^
          " is not a member of structure type " ^
           sbt bsym_table ttt ^
           "\n and trying reverse application of " ^ name ^
           " as a function also failed"
        )
      end


    | BTYP_pointer (BTYP_record ("",es)) ->
      let k = List.length es in
      let rcmp (s1,_) (s2,_) = compare s1 s2 in
      let es = List.sort rcmp es in
      let field_name = name in
      begin match list_index (List.map fst es) field_name with
      | Some n -> 
        bexpr_get_n 
        (
          btyp_pointer (List.assoc field_name es)
        )
        n te
      | None ->
      try be (EXPR_apply (sr,(e2,e)))
      with exn ->
      clierr sr
        (
          "[bind_expression] operator dot: Field " ^ name ^
          " is not a member of derefed pointer to record type " ^
           sbt bsym_table ttt ^
           "\n and trying reverse application of " ^ name ^
           " as a function also failed"
        )
      end

    (* LHS HAS A NOMINAL TYPE *)
    | BTYP_inst (i,ts') ->
      begin 
      try
        handle_field_name state bsym_table build_env env rs be bt 
          koenig_lookup cal_apply bind_type' mkenv sr e e2 name ts i ts' false
      with Not_field ->
      try be (EXPR_apply (sr,(e2,e)))
      with exn ->
        clierr sr
        (
          "[bind_expression] operator dot: Field " ^ name ^
          " is not a member of structure type " ^
           sbt bsym_table ttt ^
           "\n and trying reverse application of " ^ name ^
           " as a function also failed"
        )
      end

    (* LHS HAS A RECORD TYPE *)
    | BTYP_record ("",es) ->
      let k = List.length es in
      let rcmp (s1,_) (s2,_) = compare s1 s2 in
      let es = List.sort rcmp es in
      let field_name = name in
      begin match list_index (List.map fst es) field_name with
      | Some n -> bexpr_get_n (List.assoc field_name es) n te
      | None ->
        try be (EXPR_apply (sr,(e2,e)))
        with exn ->
        clierr sr
        (
          "[bind_expression] operator dot: Field " ^ name ^
          " is not a member of record type " ^
           sbt bsym_table ttt ^
           "\n and trying reverse application of " ^ name ^
           " as a function also failed"
        )
      end

    (* LHS OTHER ALGEBRAIC TYPE *)
    | _ ->
      begin try be (EXPR_apply (sr,(e2,e)))
      with exn ->
      clierr sr (
      "AST_dot, arg "^ string_of_expr e2^
      " is simple name, and attempt to apply it failed with " ^
      Printexc.to_string exn
      )
      end
    end

  (* RHS is an integer literal , LHS must be tuple or array *)
  | EXPR_literal (_, {Flx_literal.felix_type="int"; internal_value=s}) ->
(*
print_endline ("ASTdot, RHS is integer, LHS type is " ^ sbt bsym_table ttt);
*)
    let n = int_of_string s in
    begin match ttt with
    | BTYP_tuple ls ->
      let m = List.length ls in
      if n < 0 || n >= m then
        clierr sr ("AST_dot, tuple index "^ string_of_int n ^ 
        " out of range 0 to " ^ string_of_int (m-1) ^
        " for type " ^ sbt bsym_table ttt
        )
      else
       bexpr_get_n (List.nth ls n) n te
 
    | BTYP_array (t,BTYP_unitsum m) ->
      if n < 0 || n >= m then
        clierr sr ("AST_dot, constant array index "^ string_of_int n ^ 
        " out of range 0 to " ^ string_of_int (m-1) ^
        " for type " ^ sbt bsym_table ttt
        )
      else
       bexpr_get_n t n te
  
    | BTYP_pointer (BTYP_tuple ls) ->
      let m = List.length ls in
      if n < 0 || n >= m then
        clierr sr ("AST_dot, tuple index "^ string_of_int n ^ 
        " out of range 0 to " ^ string_of_int (m-1) ^
        " for type " ^ sbt bsym_table ttt
        )
      else
       bexpr_get_n (btyp_pointer (List.nth ls n)) n te
 
    | BTYP_pointer (BTYP_array (t,BTYP_unitsum m)) -> 
      if n < 0 || n >= m then
        clierr sr ("AST_dot, constant array index "^ string_of_int n ^ 
        " out of range 0 to " ^ string_of_int (m-1) ^
        " for type " ^ sbt bsym_table ttt
        )
      else
       bexpr_get_n (btyp_pointer t) n te
 
    | _ -> 
(*
print_endline "AST dot, can't find tuple type for standard projection, trying reverse application";
*)
      begin try be (EXPR_apply (sr,(e2,e)))
      with exn -> 
        clierr sr (
          "AST_dot, RHS " ^ string_of_expr e2 ^ 
          " is integer literal, expected LHS "^sbe bsym_table te ^"to be tuple type, got " ^
          sbt bsym_table ttt ^
          "\nReverse application also failed"
         )
      end
    end 

  (* RHS NOT A SIMPLE NAME: reverse application OR composition?? *)
  | _ ->
    begin try be (EXPR_apply (sr,(e2,e)))
    with exn ->
    (* print_endline "Reverse application failed, checking for composition"; *)
    match ttt with
    | BTYP_function (d,c) -> (* LHS *)
      (* print_endline ("AST_dot: LHS function, RHS function expr= " ^ * string_of_expr e2); *)
      begin try 
        let r,rt = be e2 in
        match rt with
        | BTYP_function (a,b) -> (* RHS *)
          (* print_endline "RHS is a function"; *)
          if c = a then (
            (* print_endline "Composed!";  *)
            bexpr_compose (btyp_function (d,b)) (te,(r,rt))
          )
          else
           clierr sr (
             "AST_dot " ^ string_of_expr e ^ " . " ^ string_of_expr e2^
             "codomain of LHS function should match domain of RHS function"
           )

        | _ -> 
          clierr sr (
          "AST_dot, LHS function, RHS arg "^ string_of_expr e2^
          " is expr, could have been a function but got " ^
          sbt bsym_table rt ^
          " and reverse application also failed."
          )
      with exn2 ->
        clierr sr (
        "AST_dot, LHS function, RHS arg "^ string_of_expr e2^
        " is expression, and attempt to bind it failed with " ^
        Printexc.to_string exn2
        )
      end

    | _ -> (* LHS not function *)
    clierr sr (
      "AST_dot, arg "^ string_of_expr e2^
      " is not simple name, and attempt to apply it failed with " ^
      Printexc.to_string exn
      )
    end
end


