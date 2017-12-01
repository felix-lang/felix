open Flx_btype
open Flx_bexpr
open Flx_ast

let cal_bind_apply 
  bsym_table state be bt env build_env
  koenig_lookup cal_apply bind_type' 
  lookup_qn_with_sig' mkenv
  inner_lookup_name_in_env eval_module_expr
  get_pub_tables 
  bind_expression'
  rs sr f' a' args
=
    let sigs = List.map snd args in
    begin (* apply *)
(*
    print_endline ("[bind_expression] GENERAL APPLY " ^ Flx_print.string_of_expr f' ^ " to " ^  Flx_print.string_of_expr a');
    print_env env;
    *)
    let (ea,ta as a) = be a' in
(*
    if not (complete_type ta) then
      print_endline ("*************>>>>>>>>> Apply argument type is not complete!!" ^ sbt bsym_table ta);
*)
    let rt t = Flx_beta.beta_reduce "flx_lookup: Expr_apply" state.Flx_lookup_state.counter bsym_table sr t in

    let ta = rt ta in
(*
    if not (complete_type ta) then
      print_endline ("*************>>>>>>>>> reduced Apply argument type is not complete!!" ^ sbt bsym_table ta);
    print_endline ("Bound argument " ^ Flx_print.sbe bsym_table a ^ " type=" ^ Flx_btype.st ta);
*)
      (* ---------------------------------------------------------- *)
      (* tie *) 
      (* ---------------------------------------------------------- *)
      try match f' with
      | EXPR_name (sr,"_tie",[]) ->
        Flx_dot.try_bind_tie bsym_table state.Flx_lookup_state.counter sr a
      | _ ->  raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->

      (* ---------------------------------------------------------- *)
      (* special case, product of functions *) 
      (* ---------------------------------------------------------- *)
      try match f' with
      | EXPR_name (_,"\\prod",[]) ->
        Flx_bind_prod.try_bind_prod bsym_table state.Flx_lookup_state.counter a
      | _ ->  raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->

      (* ---------------------------------------------------------- *)
      (* special case, sum of functions *) 
      (* ---------------------------------------------------------- *)
      try match f' with
      | EXPR_name (_,"\\sum",[]) ->
        Flx_bind_sum.try_bind_sum bsym_table state.Flx_lookup_state.counter a
      | _ ->  raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->

      (* ---------------------------------------------------------- *)
      (* special case, mediating morphism of product of functions *) 
      (* ---------------------------------------------------------- *)
      try match f' with
      | EXPR_name (_,"lrangle",[]) ->
        Flx_bind_lrangle.try_bind_lrangle bsym_table state.Flx_lookup_state.counter a
      | _ ->  raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->

      (* ---------------------------------------------------------- *)
      (* special case, mediating morphism of sum of functions *) 
      (* ---------------------------------------------------------- *)
      try match f' with
      | EXPR_name (_,"lrbrack",[]) ->
        Flx_bind_lrbrack.try_bind_lrbrack bsym_table state.Flx_lookup_state.counter a
      | _ ->  raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->

      (* This really have to be the library "int" cause we're comparing a user int literal *)
      let int_t = bt sr (TYP_name (sr,"int",[])) in
      
      (* ---------------------------------------------------------- *)
      (* special case, constant tuple projection  *) 
      (* ---------------------------------------------------------- *)
      try match f' with
      | EXPR_literal (_, {Flx_literal.felix_type="int"; internal_value=s}) ->
        let n = int_of_string s in
        Flx_dot.handle_constant_projection bsym_table sr a ta n
      | _ -> raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->
      
      (* ---------------------------------------------------------- *)
      (* special case, integer expression as tuple or array projection  *) 
      (* ---------------------------------------------------------- *)
      try 
        let f = try be f' with _ -> raise Flx_dot.OverloadResolutionError in
        if snd f = int_t then 
        begin
          match ta with
          | BTYP_array _ ->
            Flx_dot.handle_array_projection bsym_table int_t sr a ta f
          | _ -> raise Flx_dot.OverloadResolutionError
        end
        else raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->  
      
      (* ---------------------------------------------------------- *)
      (* special case, unisum expression as tuple or array projection  *) 
      (* ---------------------------------------------------------- *)
      try match f' with
      (* a dirty hack .. doesn't check unitsum is right size or type *)
      | EXPR_typed_case (sr,n,sumt) when (match bt sr sumt with | BTYP_unitsum _ -> true | _ -> false)  ->
        Flx_dot.handle_constant_projection bsym_table sr a ta n
      | _ -> raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->

      (* ---------------------------------------------------------- *)
      (* special case, name as record projection *)
      (* NOTE: this tries to handle projections of structs too *)
      (* probably shouldn't. Also does Koenig lookup and other stuff *)
      (* which might be obsolete now. *)
      (* Koenig lookup allows a function f defined in the same class *)
      (* as a struct X to be found as if it we a field name of the struct *)
      (* without requiring a qualified name *) 
      (* ---------------------------------------------------------- *)
      try match f' with
      | EXPR_name (sr, name, ts) ->
        Flx_bind_record_proj.try_bind_record_proj 
          bsym_table state build_env koenig_lookup be bt env rs cal_apply bind_type' mkenv
          f' a' a sr name ts
      | _ -> raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->
   
      (* NOW TRY DEFERED FUNCTION OVERLOADING *)
      (* ALWAYS FAILS but can set a deferred type *)
      try
        Flx_bind_deferred.set_deferred_type  
          bsym_table state env inner_lookup_name_in_env 
          eval_module_expr get_pub_tables
          rs 
          sr f' a
      with Flx_dot.OverloadResolutionError ->

      (* ---------------------------------------------------------- *)
      (* special case, array projection  *) 
      (* ---------------------------------------------------------- *)
      try
        let (bf,tf) as f = 
          try be f' 
          with 
          | Flx_exceptions.SimpleNameNotFound _ as x -> raise x
          | exn -> raise Flx_dot.OverloadResolutionError 
        in
        match tf, ta with
        (* Check for array projection *)
        | ixt1, BTYP_array (t,ixt2) when ixt1 = ixt2 -> (* SHOULD USE UNIFICATION *) 
          let prj = bexpr_aprj f ta t in
          bexpr_apply t (prj,a)
        | _ -> raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->
      (*
        print_endline ("Can't interpret apply function "^string_of_expr f'^" as projection, trying as an actual function");
      *)
      try begin (* as a function *)
      try
        let bt,tf as f =
          match Flx_typing2.qualified_name_of_expr f' with
          | Some name ->
(*
if match name with | `AST_name (_,"accumulate",_) -> true | _ -> false then begin
  print_endline "Trying to bind application of accumulate";
end;
*)
            let srn = src_of_qualified_name name in
            begin 
              try  (lookup_qn_with_sig' state bsym_table sr srn env rs name (ta::sigs))
              with 
              | Not_found -> failwith "lookup_qn_with_sig' threw Not_found"
              | exn -> raise exn 
            end
          | None ->
            begin 
              try bind_expression' state bsym_table env rs f' (a :: args) 
              with 
              | Not_found -> failwith "bind_expression' XXX threw Not_found"
              | exn -> raise exn 
            end
        in
        (*
            print_endline ("Bound f = " ^ sbe bsym_table f);
            print_endline ("tf=" ^ sbt bsym_table tf);
            print_endline ("ta=" ^ sbt bsym_table ta);
        *)
        begin match tf with
        | BTYP_effector _ 
        | BTYP_cfunction _ 
        | BTYP_function _ ->
            cal_apply state bsym_table sr rs f a 

        (* NOTE THIS CASE HASN'T BEEN CHECKED FOR POLYMORPHISM YET *)
        | BTYP_inst (i,ts',_) when
          (
            match Flx_lookup_state.hfind "flx_bind_apply" state.Flx_lookup_state.sym_table i with
            | { Flx_sym.symdef=Flx_types.SYMDEF_struct _}
            | { Flx_sym.symdef=Flx_types.SYMDEF_cstruct _} ->
              (match ta with | BTYP_record _ -> true | _ -> false)
            | _ -> false
          )
          ->
           Flx_struct_apply.cal_struct_apply 
           bsym_table state bind_type' mkenv build_env cal_apply
           rs sr f a i ts'

        | _ -> raise Flx_dot.OverloadResolutionError
        end (* tf *)
      with exn -> (* as a function failed *)
        (*
          print_endline ("Expected f to be function, got " ^ sbt bsym_table t);
        *)
        let apl name =
            be
            (
              EXPR_apply
              (
                sr,
                (
                  EXPR_name (sr,name,[]),
                  EXPR_tuple (sr,[f';a'])
                )
              )
            )
        in
        try apl "apply"
        with _ -> raise exn (* raise original error *)
      end (* as a function *)
      with exn ->
      try (* record addition *)
        match Flx_typing2.qualified_name_of_expr f' with
        | Some (`AST_name (_,"+",[])) ->
          begin match (ta::sigs) with
          | [BTYP_tuple [lt; rt]] -> 
            Flx_product_addition.add (bexpr_get_n lt 0 a) (bexpr_get_n rt 1 a)
          | [BTYP_array (et, BTYP_unitsum 2)] ->
            Flx_product_addition.add (bexpr_get_n et 0 a) (bexpr_get_n et 1 a)
          | _ -> raise Flx_dot.OverloadResolutionError
          end
        | _ ->raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError -> 
      raise exn (* tell the user about the usual case not the special cases *)
    end (* apply *)


