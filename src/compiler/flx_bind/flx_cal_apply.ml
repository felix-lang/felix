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
open Flx_name_map
open Flx_btype_occurs
open Flx_btype_subst
open Flx_bid

let debug = false


let cal_apply' 
  build_env
  state bsym_table be sr ((be1,t1) as tbe1) ((be2,t2) as tbe2) =
  let original_argument_type = t2 in

(*
print_endline ("\n%%%%%%%%\nFlx_cal_apply: Function type \n" ^ Flx_btype.st t1 ^ "\nArgument type\n`" ^ Flx_btype.st t2 ^ "\n");
*)
  let t1 = Flx_beta.beta_reduce "Flx_cal_apply:cal_apply'" state.counter bsym_table sr t1 in
  let t2 = Flx_beta.beta_reduce "Flx_cal_apply:cal_apply'" state.counter bsym_table sr t2 in

(*
print_endline ("\n%%%%%%%%\nFlx_cal_apply AFTER BETA: Function type \n" ^ Flx_btype.st t1 ^ "\nArgument type\n`" ^ Flx_btype.st t2 ^ "\n");
*) 
  let result_type,reorder =
    let funt = (* unfold "flx_cal_apply" *) t1 in  (* the UNFOLD IS BUGGED IT PUTS THE WRONG KIND ON THE FIXPOINT *)
    match funt with
    | BTYP_lineareffector(paramt,_,result_type)
    | BTYP_linearfunction (paramt,result_type)
    | BTYP_effector(paramt,_,result_type)
    | BTYP_function (paramt,result_type)
    | BTYP_cfunction (paramt,result_type) ->
      begin
(*
      if type_match bsym_table state.counter paramt t2
*)
      let rel = Flx_unify.compare_sigs bsym_table state.counter paramt t2 in
(*
      print_endline ("Function domain " ^ sbt bsym_table paramt ^ "\n " ^str_of_cmp rel ^ "\n argument type " ^ sbt bsym_table t2);
*)
      match rel with
      | `Equal -> 
(*
        print_endline "Type of function parameter agrees with type of argument";
*)
        result_type, `None
      | `Greater ->
(*
        print_endline "Type of function parameter supertype of argument, add coercion";
*)
        result_type, `Coerce (t2,paramt)
      | _ ->
      let reorder = Flx_reorder.reorder state.sym_table sr be tbe1 tbe2 in
(*
      print_endline "Type of function parameter DOES NOT agree with type of argument";
      print_endline ("Paramt = " ^ sbt bsym_table paramt ^ " paramt = " ^ sbt bsym_table t2);
*)
      begin match reorder with
      | `None ->
        clierrx "[flx_bind/flx_cal_apply.ml:74: E111] " sr
        (
          "Function " ^
          sbe bsym_table tbe1 ^
          "\nof type " ^
          sbt bsym_table t1 ^ "=" ^ Flx_btype.st t1 ^
          "\napplied to argument " ^
          sbe bsym_table tbe2 ^
          "\n of type " ^
          sbt bsym_table t2 ^ "=" ^ Flx_btype.st t2 ^
          "\nwhich doesn't agree with parameter type\n" ^
          sbt bsym_table paramt ^ "=" ^ Flx_btype.st paramt
        )
      | _ -> result_type, reorder
      end
    end (* functions *)

    (* HACKERY TO SUPPORT STRUCT CONSTRUCTORS *)
    | BTYP_inst (_,index,ts,_) ->
      begin match get_data state.sym_table index with
      { Flx_sym.id=id; symdef=entry } ->
        begin match entry with
        | SYMDEF_cstruct (cs,_,_) -> t1, `None
        | SYMDEF_struct (cs,_) -> t1, `None
        | _ ->
          clierrx "[flx_bind/flx_lookup.ml:2193: E112] " sr
          (
            "[cal_apply] Attempt to apply non-struct " ^ id ^ ", type " ^
            sbt bsym_table t1 ^
            " as constructor"
          )
        end
      end
    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:2202: E113] " sr
      (
        "Attempt to apply non-function\n" ^
        sbe bsym_table tbe1 ^
        "\nof type\n" ^
        sbt bsym_table t1 ^
        "\nto argument of type\n" ^
        sbe bsym_table tbe2
      )
  in
  (*
  print_endline
  (
    "---------------------------------------" ^
    "\nApply type " ^ sbt bsym_table t1 ^
    "\nto argument of type " ^ sbt bsym_table t2 ^
    "\nresult type is " ^ sbt bsym_table result_type ^
    "\n-------------------------------------"
  );
  *)

  let result_type = varmap_subst state.varmap result_type in
  if result_type = btyp_void () then
    clierrx "[flx_bind/flx_lookup.ml:2225: E114] " sr
    (
      "[cal_apply] Function " ^
      sbe bsym_table tbe1 ^
      "\nof type " ^
      sbt bsym_table t1 ^
      "\napplied to argument " ^
      sbe bsym_table tbe2 ^
      "\n of type " ^
      sbt bsym_table t2 ^
      "\nreturns void"
    )
  else

  (* We have to allow type variables now .. the result
  should ALWAYS be determined, and independent of function
  return type unknowns, even if that means it is a recursive
  type, perhaps like 'Fix 0' ..: we should really test
  for the *function* return type variable not being
  eliminated ..
  *)
  (*
  if var_occurs result_type
  then
    clierrx "[flx_bind/flx_lookup.ml:2249: E115] " sr
    (
      "[cal_apply] Type variable in return type applying\n" ^
        sbe bsym_table tbe1 ^
        "\nof type\n" ^
        sbt bsym_table t1 ^
        "\nto argument of type\n" ^
        sbe bsym_table tbe2
    )
  ;
  *)
  let x2 = match reorder with
  | `None -> be2,t2
  | `Coerce (arg,param) -> 
(*
    print_endline ("Coercion required from " ^ sbt bsym_table arg ^ " to " ^ sbt bsym_table param); 
*)
    bexpr_coerce ((be2,t2), param) 
  | `Reorder xs ->
    match xs with
    | [x]-> x
    | _ -> bexpr_tuple (btyp_tuple (List.map snd xs)) xs
  in
  let be2,t2 = x2 in
  let t2 = Flx_fold.minimise bsym_table state.counter t2 in
  let x2 = be2,t2 in

(*
print_endline ("ABout to bind apply result type=" ^ sbt bsym_table result_type);
print_endline ("ABout to bind apply function type=" ^ sbt bsym_table t1);
print_endline ("ABout to bind apply argument type=" ^ sbt bsym_table (snd x2));
*)
  let x = bexpr_apply result_type ((be1,t1), x2) in
  let x = match result_type with
    | BTYP_instancetype _  ->
(*
      print_endline ("Expression has instancetype " ^ Flx_print.sbe bsym_table x);
      print_endline ("Argument of application has type " ^ Flx_print.sbt bsym_table original_paramtype);
*)
      bexpr_coerce (x, original_argument_type)
      
    | BTYP_function (d,BTYP_instancetype _) ->
(*
      print_endline ("Expression has function type codomain instancetype " ^ Flx_print.sbe bsym_table x);
      print_endline ("Argument of application has type " ^ Flx_print.sbt bsym_table original_paramtype);
*)
      bexpr_coerce (x, btyp_function (d,original_argument_type))
    | _ -> x
  in  
(*
print_endline ("Bound apply = " ^ sbe bsym_table x);
*)
  x

let cal_apply 
  build_env 
  bind_expression'
  state bsym_table sr rs ((be1,t1) as tbe1) ((be2,t2) as tbe2) =
  let mkenv i = build_env state bsym_table (Some i) in
  let be i e = bind_expression' state bsym_table (mkenv i) rs e [] in
  let ((re,rt) as r) = cal_apply' build_env state bsym_table be sr tbe1 tbe2 in
  r


