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

(* 
Cases

1. f is an integer literal:
   1.1 a is a tuple: tuple projection or fail
   1.2 a is an array: tuple projection or fail
   1.3 otherwise, try appy (f,a), if that fails, fail

2. f is a unitsum literal (as above):
   2.1 a is a tuple or pointer thereto: tuple projection or fail
   2.2 a is an array or pointer thereto: tuple projection or fail
   2.3 otherwise, try appy (f,a), if that fails, fail

3. f is a constant integer slice:
   3.1 a is an array or pointer thereto: array slice or fail
   3.2 otherwise, try "apply" (f,a), if that fails, fila

4. f is a simple name
   4.1 if it is "apply", try to do a lookup, if that fails, fail
   4.2 if it is a special name, try to handle that, if that fails, fail
   4.3 otherwise there are some cases:
       4.3.1 f is "+" and the argument is a pair of records/tuples/arrays
             record/tuple/array addition
       4.3.2 its a function, so lookup with sig, if that fails fail
             (functions include projections, injections, C funs,
             etc etc)
       4.3.3 its a struct: lookup _ctor_name, if that fails, fail
       4.3.4 its a non-function, so try "apply"

5. f is a qualified name
   As above more or less .. (except + case)

6. f is a suffixed name
   6.1 it must be a function or struct, lookup with suffix
       if that fails, fail, otherwise check the argument
       agrees with the suffix, if not add a coercion
7. f is an integer expression:
   7.1 if the argument is an array, try a projection
   7.2 fail is tuple
   7.3 otherwise try "apply"


*)


  
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
    print_env env;
    *)
    let (ea,ta as a) = be a' in
(*
    print_endline ("[bind_expression] GENERAL APPLY " ^ 
      Flx_print.string_of_expr f' ^ " to " ^  Flx_print.string_of_expr a' ^ ", type= " ^ Flx_print.sbt bsym_table ta
    );
*)
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
      | `EXPR_name (sr,name,[]) ->
        begin 
          match generic_function_dispatcher bsym_table state.Flx_lookup_state.counter sr name a with 
          | Some x -> x
          | None ->raise Flx_exceptions.TryNext (* not a known generic *) 
         end
      | _ -> raise Flx_exceptions.TryNext (* function wasn't a name *)
      with Flx_exceptions.TryNext ->
   
 
      (* ---------------------------------------------------------- *)
      (* special case, name as record projection *)
      (* NOTE: this tries to handle projections of structs too *)
      (* probably shouldn't.  *)
      (* ---------------------------------------------------------- *)
      try match f' with
      | `EXPR_name (sr, name, ts) ->
        Flx_bind_record_proj.try_bind_record_proj 
          bsym_table state build_env be bt env rs cal_apply bind_type' mkenv
          f' a' a sr name ts
      | _ -> raise Flx_exceptions.TryNext
      with Flx_exceptions.TryNext ->
   
      try
        Flx_bind_inline_projection.bind_inline_projection bsym_table be bt sr f' a' ta a  
      with Flx_exceptions.TryNext ->
      try
        Flx_bind_slice.bind_constant_slice bsym_table be bt sr f' a' ta a 
      with Flx_exceptions.TryNext ->


      (*
        print_endline ("Can't interpret apply function "^string_of_expr f'^" as projection, trying as an actual function");
      *)
      try begin (* as a function *)
      try
        let bt,tf as f =
          match Flx_typing2.qualified_name_of_expr f' with
          | Some name ->
(*
print_endline ("Checking if " ^ Flx_print.string_of_qualified_name name ^ " is a function");
if match name with | `AST_name (_,"accumulate",_) -> true | _ -> false then begin
  print_endline "Trying to bind application of accumulate";
end;
*)
            let srn = src_of_qualified_name name in
            begin 
              try  
                 let result = (lookup_qn_with_sig' state bsym_table sr srn env rs name (ta::sigs)) in
(*
print_endline ("yes, got " ^ Flx_print.sbe bsym_table result);
print_endline ("  WITH TYPE " ^ Flx_print.sbt bsym_table (snd result));
*)
                 result
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
            print_endline ("Bound f = " ^ Flx_print.sbe bsym_table f);
            print_endline ("tf=" ^ Flx_print.sbt bsym_table tf);
            print_endline ("ta=" ^ Flx_print.sbt bsym_table ta);
*)
        begin match tf with
        | BTYP_lineareffector _ 
        | BTYP_effector _ 
        | BTYP_function _ 
        | BTYP_linearfunction _
        | BTYP_cfunction _ 
          ->
          begin try
(*
           print_endline (" ** Bound LHS of application as function!");
*)
           let result =  cal_apply state bsym_table sr rs f a  in
(*
           print_endline (" ** application of function done!");
*)
           result
          with exn -> 
(*
            print_endline ("!!! cal_apply failed");
*)
            raise exn
          end
        (* NOTE THIS CASE HASN'T BEEN CHECKED FOR POLYMORPHISM YET *)
        | BTYP_inst (_,i,ts',_) ->
(*
          print_endline (" ** Bound LHS of application and a nominal type");
*)
          if 
          (
            match Flx_lookup_state.hfind "flx_bind_apply" state.Flx_lookup_state.sym_table i with
            | { Flx_sym.symdef=Flx_types.SYMDEF_struct _}
            | { Flx_sym.symdef=Flx_types.SYMDEF_cstruct _} ->
              (match ta with | BTYP_record _ -> true | _ -> false)
            | _ -> false
          )
          then begin 
(*
            print_endline (" ** a struct with a record argument");
*)
            Flx_struct_apply.cal_struct_apply_to_record
            bsym_table state bind_type' mkenv build_env cal_apply
            rs sr f a i ts'
          end else begin
(*
            print_endline ("  ** Not a struct with a record argument");
*)
            raise Flx_dot.OverloadResolutionError
          end

        | _ -> raise Flx_dot.OverloadResolutionError
        end (* tf *)
      with exn -> (* as a function failed *)
        (*
          print_endline ("Expected f to be function, got " ^ sbt bsym_table t);
        *)
        begin try 
          begin match f' with
          | `EXPR_name (_,"apply",_) ->
(*
            print_endline ("Application of `apply` function failed: terminating RECURSION?");
*)
            raise exn
          | _ ->
(*
            print_endline ("RECURSION: Application failed, trying to bind:");
*)
            let x = `EXPR_apply ( sr, ( `EXPR_name (sr,"apply",[]), `EXPR_tuple (sr,[f';a']))) in
(*
            print_endline ("  expr = " ^ Flx_print.string_of_expr x);
*)
            be x
          end
        with _ -> 
(*
print_endline ("apply function failed too f'=" ^ Flx_print.string_of_expr f');
*)
          raise exn (* raise original error *)
        end
      end (* as a function *)
      with exn ->

      begin try (* record addition *)
(*
        print_endline ("Finally, trying record addition");
*)
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
(*
        print_endline ("Record addition failed, raising original error");
*)
        raise exn (* tell the user about the usual case not the special cases *)
      end (* record addition *)

    end (* apply *)


