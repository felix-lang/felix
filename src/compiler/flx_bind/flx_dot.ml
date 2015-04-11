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

exception OverloadResolutionError

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




let handle_constant_projection bsym_table sr a ta n =
  begin match unfold "flx_lookup" ta with
  | BTYP_tuple ls ->
    let m = List.length ls in
    if n < 0 || n >= m then
      clierr sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
     bexpr_get_n (List.nth ls n) n a

  | BTYP_array (t,BTYP_unitsum m) ->
    if n < 0 || n >= m then
      clierr sr ("AST_dot, constant array index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
     bexpr_get_n t n a

  | BTYP_pointer (BTYP_tuple ls) ->
    let m = List.length ls in
    if n < 0 || n >= m then
      clierr sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
     bexpr_get_n (btyp_pointer (List.nth ls n)) n a

  | BTYP_pointer (BTYP_array (t,BTYP_unitsum m)) -> 
    if n < 0 || n >= m then
      clierr sr ("AST_dot, constant array index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
     bexpr_get_n (btyp_pointer t) n a

  | _ -> raise OverloadResolutionError
  end

let handle_array_projection bsym_table int_t sr a ta n =
  let n = 
    let ixt = match unfold "flx_lookup" ta with
      | BTYP_array (_,ixt)
      | BTYP_pointer (BTYP_array (_,ixt)) -> ixt
      | _ -> assert false
    in
    if snd n = int_t then bexpr_coerce (n,ixt)
    else n
  in
  match unfold "flx_lookup" ta with
  | BTYP_array (vt,ixt) ->
    assert (snd n = ixt);
    bexpr_apply vt (bexpr_aprj n ta vt, a)

  | BTYP_pointer (BTYP_array (vt,ixt)) ->
    assert (snd n = ixt);
    bexpr_apply (BTYP_pointer vt) (bexpr_aprj n ta vt, a)
  | _ -> assert false

