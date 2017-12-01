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
(*
  if t1 <> t1' || t2 <> t2' then begin
print_endline ("cal_apply' BEFORE NORMALISE, fn = " ^ sbt bsym_table t1' ^ " arg=" ^ sbt bsym_table t2');
print_endline ("cal_apply', AFTER NORMALISE, fn = " ^ sbt bsym_table t1 ^ " arg=" ^ sbt bsym_table t2);
  end
  ;
*)
  let rest,reorder =
    match unfold "flx_lookup" t1 with
    | BTYP_effector(argt,_,rest)
    | BTYP_function (argt,rest)
    | BTYP_cfunction (argt,rest) ->
      begin
(*
      if type_match bsym_table state.counter argt t2
*)
      let rel = Flx_unify.compare_sigs bsym_table state.counter argt t2 in
      match rel with
      | `Equal -> rest, `None
(*
        print_endline "Type of function parameter agrees with type of argument";
*) 
      | `Greater ->
(*
        print_endline "Type of function parameter supertype of argument";
*)
        rest, `Coerce (t2,argt)
      | _ ->
      let reorder = Flx_reorder.reorder state.sym_table sr be tbe1 tbe2 in
(*
      print_endline "Type of function parameter DOES NOT agree with type of argument";
      print_endline ("Paramt = " ^ sbt bsym_table argt ^ " argt = " ^ sbt bsym_table t2);
*)
      begin match reorder with
      | `None ->
        clierrx "[flx_bind/flx_lookup.ml:2170: E111] " sr
        (
          "[cal_apply] Function " ^
          sbe bsym_table tbe1 ^
          "\nof type " ^
          sbt bsym_table t1 ^ "=" ^ Flx_btype.st t1 ^
          "\napplied to argument " ^
          sbe bsym_table tbe2 ^
          "\n of type " ^
          sbt bsym_table t2 ^ "=" ^ Flx_btype.st t2 ^
          "\nwhich doesn't agree with parameter type\n" ^
          sbt bsym_table argt ^ "=" ^ Flx_btype.st argt
        )
      | _ -> rest, reorder
      end
    end (* functions *)

    (* HACKERY TO SUPPORT STRUCT CONSTRUCTORS *)
    | BTYP_inst (index,ts,_) ->
      begin match get_data state.sym_table index with
      { Flx_sym.id=id; symdef=entry } ->
        begin match entry with
        | SYMDEF_cstruct (cs,_) -> t1, `None
        | SYMDEF_struct (cs) -> t1, `None
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
    "\nresult type is " ^ sbt bsym_table rest ^
    "\n-------------------------------------"
  );
  *)

  let rest = varmap_subst state.varmap rest in
  if rest = btyp_void () then
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
  if var_occurs rest
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
print_endline ("ABout to bind apply result type=" ^ sbt bsym_table rest);
print_endline ("ABout to bind apply function type=" ^ sbt bsym_table t1);
print_endline ("ABout to bind apply argument type=" ^ sbt bsym_table (snd x2));
*)
  let x = bexpr_apply rest ((be1,t1), x2) in
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


