open Flx_btype
open Flx_bexpr
open Flx_ast

(* generic function dispatcher *)
let generic_function_dispatcher bsym_table counter sr f a =
  match f with
  (* ---------------------------------------------------------- *)
  (* tie *) 
  (* ---------------------------------------------------------- *)
  | "_tie" -> Some (Flx_dot.try_bind_tie bsym_table counter sr a)

  (* ---------------------------------------------------------- *)
  (* special case, product of functions *) 
  (* ---------------------------------------------------------- *)
  | "\\prod" -> Some (Flx_bind_prod.try_bind_prod bsym_table counter a)

  (* ---------------------------------------------------------- *)
  (* special case, sum of functions *) 
  (* ---------------------------------------------------------- *)
  | "\\sum" -> Some ( Flx_bind_sum.try_bind_sum bsym_table counter a)

  (* ---------------------------------------------------------- *)
  (* special case, mediating morphism of product of functions *) 
  (* ---------------------------------------------------------- *)
  | "lrangle" -> Some (Flx_bind_lrangle.try_bind_lrangle bsym_table counter a)

  (* ---------------------------------------------------------- *)
  (* special case, mediating morphism of sum of functions *) 
  (* ---------------------------------------------------------- *)
  | "lrbrack" -> Some (Flx_bind_lrbrack.try_bind_lrbrack bsym_table counter a)

  | _ -> None


(* Note: the exception chaining machinery below is a superior HACK

  WHat happens is that it just tries each case in turn until
  one works, otherwise finally throws an OverloadResolutionError

  This sucks but it is used because of a weakness in Ocaml that
  its impossible to figure out deeply nested scopes, the code
  gets too indented: the method used linearises the cases.

  What SHOULD happen is that in most cases if there is a failure
  it is final, what actually happens is that we merge a failure
  to select the special case with an error handling the case
  when it is selected. 

  This does allow user overrides however.

  Consider:

    2 (1,2,3) vs 2 5

  The first case is a tuple projection, the second could be
  a user defined multiply using "apply" function. If we try
  for a projection and fail, we can fall back to the user
  apply function.

  However in this case the user is not allowed to do an
  apply of an int on a tuple so if we actually have a tuple
  argument, we should fail outright and not fall back.

  The problem is the specialised evaluators just fail
  without a specifying why.

*)

exception TryNext
  
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
      (* generic function *) 
      (* ---------------------------------------------------------- *)
      try match f' with
      | EXPR_name (sr,name,[]) ->
        begin 
          match generic_function_dispatcher bsym_table state.Flx_lookup_state.counter sr name a with 
          | Some x -> x
          | None ->raise TryNext (* not a known generic *) 
         end
      | _ -> raise TryNext (* function wasn't a name *)
      with TryNext ->
   
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
      (* special case, constant slice of tuple *) 
      (* ---------------------------------------------------------- *)
      try 
        match ta with
        | BTYP_tuple ls ->
          let tmin = 0 and tmax = List.length ls - 1 in
          let smin, smax = 
          match f' with
            | EXPR_name (_,"Slice_all",[]) -> tmin,tmax
            | EXPR_name (_,"Slice_none",[]) -> tmax,tmin
            | EXPR_apply(_,(EXPR_name (_,"Slice_from",[]), EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) ->
              int_of_string v1,tmax
            | EXPR_apply(_,(EXPR_name (_,"Slice_from_counted",[]),EXPR_tuple(_,[EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1});EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v2})]))) -> (* second arg is count *)
              int_of_string v1,int_of_string v1+int_of_string v2-1
            | EXPR_apply(_,(EXPR_name (_,"Slice_to_incl",[]),EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) ->
              0,int_of_string v1
            | EXPR_apply(_,(EXPR_name (_,"Slice_to_excl",[]),EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) ->
              0,int_of_string v1 - 1
            | EXPR_apply(_,(EXPR_name (_,"Slice_range_incl",[]),EXPR_tuple(_,[EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1});EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v2})])))->
              int_of_string v1,int_of_string v2
            | EXPR_apply(_,(EXPR_name (_,"Slice_range_excl",[]),EXPR_tuple(_,[EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1});EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v2})])))->
              int_of_string v1,int_of_string v2 - 1
            | EXPR_apply(_,(EXPR_name (_,"Slice_one",[]),EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) -> 
              int_of_string v1,int_of_string v1
            | _ -> raise Flx_dot.OverloadResolutionError
          in
          let smin = max tmin smin and smax = min tmax smax  in
          let sls = ref [] in
          for i = smin to smax do
            sls := i :: !sls
          done;
          let sls = List.rev_map (fun i -> EXPR_get_n (sr,(i,a'))) !sls in
          be (EXPR_tuple (sr,sls))
        | _ -> raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->

      (* ---------------------------------------------------------- *)
      (* special case, constant slice of tuple *) 
      (* ---------------------------------------------------------- *)
      try 
        match ta with
        | BTYP_pointer (BTYP_tuple ls) ->
          let tmin = 0 and tmax = List.length ls - 1 in
          let smin, smax = 
          match f' with
            | EXPR_name (_,"Slice_all",[]) -> tmin,tmax
            | EXPR_name (_,"Slice_none",[]) -> tmax,tmin
            | EXPR_apply(_,(EXPR_name (_,"Slice_from",[]), EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) ->
              int_of_string v1,tmax
            | EXPR_apply(_,(EXPR_name (_,"Slice_from_counted",[]),EXPR_tuple(_,[EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1});EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v2})]))) -> (* second arg is count *)
              int_of_string v1,int_of_string v1+int_of_string v2-1
            | EXPR_apply(_,(EXPR_name (_,"Slice_to_incl",[]),EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) ->
              0,int_of_string v1
            | EXPR_apply(_,(EXPR_name (_,"Slice_to_excl",[]),EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) ->
              0,int_of_string v1 - 1
            | EXPR_apply(_,(EXPR_name (_,"Slice_range_incl",[]),EXPR_tuple(_,[EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1});EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v2})])))->
              int_of_string v1,int_of_string v2
            | EXPR_apply(_,(EXPR_name (_,"Slice_range_excl",[]),EXPR_tuple(_,[EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1});EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v2})])))->
              int_of_string v1,int_of_string v2 - 1
            | EXPR_apply(_,(EXPR_name (_,"Slice_one",[]),EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) -> 
              int_of_string v1,int_of_string v1
            | _ -> raise Flx_dot.OverloadResolutionError
          in
          let smin = max tmin smin and smax = min tmax smax  in
          let sls = ref [] in
          for i = smin to smax do
            sls := i :: !sls
          done;
          let sls = List.rev_map (fun i -> EXPR_get_n (sr,(i,a'))) !sls in
          be (EXPR_tuple (sr,sls))
        | _ -> raise Flx_dot.OverloadResolutionError
      with Flx_dot.OverloadResolutionError ->

      (* ---------------------------------------------------------- *)
      (* special case, integer expression as tuple or array projection  *) 
      (* ---------------------------------------------------------- *)
      try 
        let f = try be f' with _ -> raise Flx_dot.OverloadResolutionError in
        let int_t = bt sr (TYP_name (sr,"int",[])) in
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
      (* special case, unitsum expression as tuple or array projection  *) 
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


