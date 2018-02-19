
(*
This module is responsible for generating functional wrappers for 
constructions which are used as function values requiring
a closure, but which are not functions. For example union constructors
are not functions, but Felix allows them to be used as fuctions
because they take arguments. They can be converted to functions
on demand by simply wrapping them inside a function. In effect
this is lambda lifting done late.

Certain other terms represent functions but are not in the right
form to make a closure, in particular the series composition 
operator is intended to represent a function but needs a wrapper
to effect this.

This module generates wrappers as required and replaces cases of
closures of non-function entities with closures over the generated
wrappers.
*)
open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
open Flx_bbdcl
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_unify
open Flx_exceptions
open Flx_use
open Flx_prop
open Flx_bid
open Flx_btype_subst

let noeffects = Flx_btype.btyp_unit ()

type closure_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  wrappers : (bid_t, bid_t) Hashtbl.t;
}


let make_closure_state syms =
  {
    syms = syms;
    wrappers = Hashtbl.create 97;
  }


let make_inner_function state bsym_table closure_bid sr vs ts ps =
   assert (List.length ts = List.length vs);
  (* Make the type of the closed value. *)
  let closed_type = match ps with 
    | [typ] -> tsubst sr vs ts typ
    | typs -> btyp_tuple (List.map (tsubst sr vs ts) typs) 
  in

  (* Make the closed value that's hidden inside our wrapper function. *)
  let closed_bid = fresh_bid state.syms.counter in
  let closed_name = "_a" ^ string_of_bid closed_bid in
  let closed_val = bbdcl_val ([],closed_type,`Val) in

  Flx_bsym_table.add bsym_table closed_bid (Some closure_bid)
    (Flx_bsym.create ~sr closed_name closed_val);

  (* Make the parameters for the wrapper function. *)
  let param =
    { pkind=`PVal;
      pid=closed_name;
      pindex=closed_bid;
      ptyp=closed_type }
  in

  (* Make the argument that we'll pass to our wrapped function. *)
  let arg = bexpr_varname closed_type (closed_bid, []) in

  (* Return a couple parameters *)
  param, arg

(*
let gen_composite_closure_entry state bsym_table sr (f1,t1) (f2,t2) =
  let vs = [] in (* HACK, temporary, WRONG *)
  let ts = [] in (* HACK, temporary, WRONG *)
  let a1t,r1t,a2t,r2t = match t1, t2 with
  | BTYP_function (a1t,r1t), BTYP_function (a2t,r2t) -> a1t,r1t,a2t,r2t
  | _ -> assert false
  in
  (* Make a bid for our closure wrapper function. *)
  let closure_bid = fresh_bid state.syms.counter in

  (* Make the wrapper function parameter variable. *)
  let p_bid = fresh_bid state.syms.counter in
  let p_name = "_a" ^ string_of_bid p_bid in
  let p_val = bbdcl_val (vs,a1t,`Val) in

  (* Make the parameters for the wrapper function. *)
  let param =
    { pkind=`PVal;
      pid=p_name;
      pindex=p_bid;
      ptyp=a1t}
  in

  (* Make the argument that we'll pass to our wrapped function. *)
  let arg = bexpr_varname a1t (p_bid, []) in

  (* the instructions of the function *)
  let exes =
    let e1 = bexpr_apply r1t ((f1,t1), arg) in
    let e2 = bexpr_apply r2t ((f2,t2), e1) in
    [ bexe_fun_return (sr, e2) ]
  in

  (* the function record *)
  let bbdcl = bbdcl_fun ([],vs,(Satom param,None),r2t,exes) in

  (* the complete symbol *)
  let bsym = Flx_bsym.create ~sr:sr ("_a" ^ string_of_int closure_bid ) (bbdcl) in

  (* now add it to the table *)
  Flx_bsym_table.add bsym_table closure_bid None bsym;

  (* add the parameter afterwards so its parent exists *)
  Flx_bsym_table.add bsym_table p_bid (Some closure_bid)
    (Flx_bsym.create ~sr p_name p_val)
  ;

(* return the index of the wrapper generated *)
  closure_bid,ts
 
*)

let rec chk_expr state bsym_table nutab sr exe e : Flx_bexpr.t = 
(*
print_endline ("Closure wrapper generator: Check expr " ^ sbe bsym_table e);
*)
  let sbx exe = string_of_bexe bsym_table 0 exe in
  let ce e = chk_expr state bsym_table nutab sr exe e in 
  match e with
  | BEXPR_apply ((BEXPR_closure (i,ts),ft),e),rt ->
    begin
      let bsym = Flx_bsym_table.find bsym_table i in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_external_fun _ -> bexpr_apply_prim rt (i,ts,ce e)
      | BBDCL_struct _  -> bexpr_apply_struct rt (i,ts,ce e)
      | BBDCL_cstruct  _  -> bexpr_apply_struct rt (i,ts,ce e)
      | BBDCL_nonconst_ctor  _  -> 
(*
        print_endline ("Closure of non-const ctor");
*)
        bexpr_apply_struct rt (i,ts,ce e)

      | BBDCL_fun _ -> bexpr_apply_direct rt (i,ts,ce e)
      | _ -> assert false 
    end

  | BEXPR_apply ((BEXPR_inj (n,d,c),ft) as f,e),rt ->
    bexpr_apply rt (f,ce e)

  | BEXPR_apply ((BEXPR_prj (n,d,c),ft) as f,e),rt ->
    bexpr_apply rt (f,ce e)

  | BEXPR_apply ((BEXPR_aprj (ix,d,c),ft),e),rt ->
    let ix = ce ix in
    bexpr_apply rt (bexpr_aprj ix d c,ce e)


  | BEXPR_apply (f,e),t -> 
    bexpr_apply t (ce f, ce e)

  | BEXPR_closure (i,ts),t ->
    begin
      let bsym = Flx_bsym_table.find bsym_table i in
      let fsr = Flx_bsym.sr bsym in 
      match Flx_bsym.bbdcl bsym with
      | BBDCL_external_fun (_,_,_,_,_,_,`Callback _) ->
(*
        print_endline ("in exe=" ^ sbx exe ^ " \nCallback passed as argument [not allowed?] " ^ sbe bsym_table e);
*)
        e

      | BBDCL_fun (props,_,_,_,_,_) when List.mem `Cfun props -> 
(*
        print_endline ("in exe=" ^ sbx exe ^ "\nCfun function pointer passed as argument? " ^ sbe bsym_table e);
*)
        e

      | BBDCL_external_fun (props,vs,ps,ret,_,_,_) ->
(*
if ts <> [] then begin
        print_endline ("in exe=" ^ sbx exe ^ "\nPrimitive function passed as argument " ^ sbe bsym_table e);
       print_endline ("ts=" ^ catmap "," (sbt bsym_table) ts);
end;
*)
 
(*
        assert (ts = []);
*)
        let closure_bid = fresh_bid state.syms.counter in
        let closure_name = ("_a" ^ string_of_int closure_bid ^ "_" ^ Flx_bsym.id bsym) in
        Flx_bsym_table.add nutab closure_bid None 
          (Flx_bsym.create ~sr:fsr closure_name (bbdcl_invalid ()))
        ;
        let param, arg = make_inner_function state nutab closure_bid fsr vs ts ps in

        (* Generate a call to the wrapped function. *)
        let exes =
          match ret with
          | BTYP_void ->
              [ bexe_call_prim (fsr, i, ts, arg);
                bexe_proc_return fsr ]
          | _ ->
              let e = bexpr_apply_prim ret (i, ts, arg) in
              [ bexe_fun_return (fsr, e) ]
        in

        let props = if List.mem `Generator props then [`Generator] else [] in
        let bbdcl = bbdcl_fun (props,[],(Satom param,None),ret,noeffects,exes) in
        Flx_bsym_table.update_bbdcl nutab closure_bid bbdcl;
        bexpr_closure t (closure_bid, [])

      | BBDCL_struct (vs,ps) ->
(*
        print_endline ("in exe=" ^ sbx exe ^ "\nStruct passed as argument " ^ sbe bsym_table e);
*)
(*
        assert (ts = []);
*)
        let closure_bid = fresh_bid state.syms.counter in
        let closure_name = ("_a" ^ string_of_int closure_bid ^ "_" ^ Flx_bsym.id bsym) in
        Flx_bsym_table.add nutab closure_bid None 
          (Flx_bsym.create ~sr:fsr closure_name (bbdcl_invalid ()))
        ;
        let param, arg = make_inner_function state nutab closure_bid fsr vs ts (List.map snd ps) in

        (* Generate a call to the wrapped function. *)
        let ret = btyp_inst (i,ts,Flx_kind.KIND_type) in 
        let exes =
           let e = bexpr_apply_struct ret (i, ts, arg) in
           [ bexe_fun_return (fsr, e) ]
        in
        let bbdcl = bbdcl_fun ([],[],(Satom param,None),ret,noeffects,exes) in
        Flx_bsym_table.update_bbdcl nutab closure_bid bbdcl;
(*
print_endline ("Struct ctor wrapper: constructor type = " ^ sbt bsym_table t);
print_endline ("Struct wrapper: struct type = " ^ sbt bsym_table ret);
*)
        bexpr_closure t (closure_bid, [])

      | BBDCL_cstruct (vs,ps,_) ->
(*
        print_endline ("in exe=" ^ sbx exe ^ "\nCstruct passed as argument " ^ sbe bsym_table e);
*)
(*
        assert (ts = []);
*)
        let closure_bid = fresh_bid state.syms.counter in
        let closure_name = ("_a" ^ string_of_int closure_bid ^ "_" ^ Flx_bsym.id bsym) in
        Flx_bsym_table.add nutab closure_bid None 
          (Flx_bsym.create ~sr:fsr closure_name (bbdcl_invalid ()))
        ;
        let param, arg = make_inner_function state nutab closure_bid fsr vs ts (List.map snd ps) in

        (* Generate a call to the wrapped function. *)
        let ret = btyp_inst (i,ts,Flx_kind.KIND_type) in 
        let exes =
           let e = bexpr_apply_struct ret (i, ts, arg) in
           [ bexe_fun_return (fsr, e) ]
        in

        let bbdcl = bbdcl_fun ([],[],(Satom param,None),ret,noeffects,exes) in
        Flx_bsym_table.update_bbdcl nutab closure_bid bbdcl;
        bexpr_closure t (closure_bid, [])

      | BBDCL_nonconst_ctor (vs,_,ret,_,p,_,_) as foo ->
(*
        print_endline ("in exe=" ^ sbx exe ^ "\nNon-const union constructor passed as argument" ^ sbe bsym_table e);
*)
(*
        assert (ts = []);
*)
        let closure_bid = fresh_bid state.syms.counter in
        let closure_name = ("_a" ^ string_of_int closure_bid ^ "_" ^ Flx_bsym.id bsym) in
        Flx_bsym_table.add nutab closure_bid None 
          (Flx_bsym.create ~sr:fsr closure_name (bbdcl_invalid ()))
        ;
        let param, arg = make_inner_function state nutab closure_bid fsr vs ts [p] in

        (* Generate a call to the wrapped function. *)
        let exes =
           let e = bexpr_apply_struct ret (i, ts, arg) in
           [ bexe_fun_return (fsr, e) ]
        in

        let bbdcl = bbdcl_fun ([],[],(Satom param,None),ret,noeffects,exes) in
        Flx_bsym_table.update_bbdcl nutab closure_bid bbdcl;
        bexpr_closure t (closure_bid, [])


      | _ -> e
    end 

  (* represents f1 (f2 x) i.e. second function applied first *)
  | BEXPR_compose (f1,f2),t ->
    print_endline ("in exe=" ^ sbx exe ^ "\nComposition passed as argument " ^ sbe bsym_table e);
    let f1 = ce f1 in
    let f2 = ce f2 in
    let closure_bid = fresh_bid state.syms.counter in
    let closure_name = ("_a" ^ string_of_int closure_bid ^ "_strtyp") in
    Flx_bsym_table.add nutab closure_bid None 
      (Flx_bsym.create ~sr:sr closure_name (bbdcl_invalid ()))
    ;
    let dom2,cod2= 
      match f2 with 
      | _,BTYP_function (d,c) -> d,c
      | _ -> assert false 
    in
    let dom1,cod1 = 
      match f1 with
      | _,BTYP_function (d,c) -> d,c
      | _ -> assert false
    in
    assert (cod2 = dom1); (* should use type_eq here .. *)
    assert (cod1 = t);
    let param, arg = make_inner_function state nutab closure_bid sr [] [] [dom2] in

    (* Generate a call to the wrapped function. *)
    let exes =
       let e = bexpr_apply cod2 (f2, arg) in
       let e = bexpr_apply t (f1, e) in
       [ bexe_fun_return (sr, e) ]
    in

    let bbdcl = bbdcl_fun ([],[],(Satom param,None),cod1,noeffects,exes) in
    Flx_bsym_table.update_bbdcl nutab closure_bid bbdcl;
    bexpr_closure t (closure_bid, [])

  (* in the cltpointer projection the domain and codomain value types
     are the compact linear types of the values the pointer points AT.
     The type of the projection is a map from a cltpointer to another cltpointer
     where the pointer codomain types agree with the comain and codomain
     value types of the projection.

     The projection itself is independent of the base domain type
     of the pointer. However, as a function, it maps pointers to
     pointers and the base domain type of these pointers is the
     same, and must be known. It can only be recovered from
     the type of the projection.
  *)
  | BEXPR_cltpointer_prj (domain_value_type,codomain_value_type,divisor),
    (BTYP_function(
      (BTYP_cltpointer (base_type,domain_value_type2) as fdomain),
      (BTYP_cltpointer (base_type2, codomain_value_type2) as fcodomain)
    ) as t) as x ->
    print_endline ("in exe=" ^ sbx exe ^ "\nCLT Projection passed as argument " ^ sbe bsym_table e);
    let closure_bid = fresh_bid state.syms.counter in
    let closure_name = ("_a" ^ string_of_int closure_bid ^ "_strtyp") in
    Flx_bsym_table.add nutab closure_bid None 
      (Flx_bsym.create ~sr:sr closure_name (bbdcl_invalid ()))
    ;
    let param, arg = make_inner_function state nutab closure_bid sr [] [] [fdomain] in

    (* Generate a call to the wrapped function. *)
    let exes =
       let e = bexpr_apply fcodomain (x, arg) in
       [ bexe_fun_return (sr, e) ]
    in

    let bbdcl = bbdcl_fun ([],[],(Satom param,None),fcodomain,noeffects,exes) in
    Flx_bsym_table.update_bbdcl nutab closure_bid bbdcl;
    bexpr_closure t (closure_bid, [])


  | BEXPR_prj (n,d,c),t as x ->
(*
    print_endline ("in exe=" ^ sbx exe ^ "\nProjection passed as argument " ^ sbe bsym_table e);
*)
    let closure_bid = fresh_bid state.syms.counter in
    let closure_name = ("_a" ^ string_of_int closure_bid ^ "_strtyp") in
    Flx_bsym_table.add nutab closure_bid None 
      (Flx_bsym.create ~sr:sr closure_name (bbdcl_invalid ()))
    ;
    let param, arg = make_inner_function state nutab closure_bid sr [] [] [d] in

    (* Generate a call to the wrapped function. *)
    let exes =
       let e = bexpr_apply c (x, arg) in
       [ bexe_fun_return (sr, e) ]
    in

    let bbdcl = bbdcl_fun ([],[],(Satom param,None),c,noeffects,exes) in
    Flx_bsym_table.update_bbdcl nutab closure_bid bbdcl;
    bexpr_closure t (closure_bid, [])


  | BEXPR_inj (n,d,c),t as x ->
(*
    print_endline ("in exe=" ^ sbx exe ^ "\nInjection passed as argument " ^ sbe bsym_table e);
*)
    let closure_bid = fresh_bid state.syms.counter in
    let closure_name = ("_a" ^ string_of_int closure_bid ^ "_strtyp") in
    Flx_bsym_table.add nutab closure_bid None 
      (Flx_bsym.create ~sr:sr closure_name (bbdcl_invalid ()))
    ;
    let param, arg = make_inner_function state nutab closure_bid sr [] [] [d] in

    (* Generate a call to the wrapped function. *)
    let exes =
       let e = bexpr_apply c (x, arg) in
       [ bexe_fun_return (sr, e) ]
    in

    let bbdcl = bbdcl_fun ([],[],(Satom param,None),c,noeffects,exes) in
    Flx_bsym_table.update_bbdcl nutab closure_bid bbdcl;
    bexpr_closure t (closure_bid, [])

  | BEXPR_aprj (idx,d,c),t as x->
    print_endline ("in exe=" ^ sbx exe ^ "\nArray projection passed as argument " ^ sbe bsym_table e);
    let idx = ce idx in
    let closure_bid = fresh_bid state.syms.counter in
    let closure_name = ("_a" ^ string_of_int closure_bid ^ "_strtyp") in
    Flx_bsym_table.add nutab closure_bid None 
      (Flx_bsym.create ~sr:sr closure_name (bbdcl_invalid ()))
    ;
    let param, arg = make_inner_function state nutab closure_bid sr [] [] [d] in

    (* Generate a call to the wrapped function. *)
    let exes =
       let p = bexpr_aprj idx d c in
       let e = bexpr_apply c (p, arg) in
       [ bexe_fun_return (sr, e) ]
    in

    let bbdcl = bbdcl_fun ([],[],(Satom param,None),c,noeffects,exes) in
    Flx_bsym_table.update_bbdcl nutab closure_bid bbdcl;
    bexpr_closure t (closure_bid, [])

  | BEXPR_identity_function t,_ ->
(*
print_endline ("[flx_mkcls2] Generating identity for " ^ sbt bsym_table t);
*)
    let ft = btyp_function (t,t) in
    let closure_bid = fresh_bid state.syms.counter in
    let closure_name = ("_id_" ^ string_of_int closure_bid) in
    Flx_bsym_table.add nutab closure_bid None 
      (Flx_bsym.create ~sr:sr closure_name (bbdcl_invalid ()))
    ;

    let param, arg = make_inner_function state nutab closure_bid sr [] [] [t] in
    let exes = [bexe_fun_return (sr,arg) ] in
    let bbdcl = bbdcl_fun ([],[],(Satom param,None),t,noeffects,exes) in
    Flx_bsym_table.update_bbdcl nutab closure_bid bbdcl;

    bexpr_closure ft (closure_bid, [])


  | e -> 
    let e = Flx_bexpr.map ~f_bexpr:ce e in
    e

let chk_exe state bsym_table nutab exe =
  let ce sr e = chk_expr state bsym_table nutab sr exe e in
  match exe with
  | BEXE_call (sr,(BEXPR_closure (i,ts),t),e2) -> 
    begin
      let bsym = Flx_bsym_table.find bsym_table i in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_external_fun _ -> bexe_call_prim (sr,i,ts,ce sr e2) 
      | BBDCL_fun _  -> bexe_call_direct (sr,i,ts, ce sr e2)
      | _ -> assert false
    end

  | BEXE_call (sr,e1,e2) -> 
    bexe_call (sr, ce sr e1, ce sr e2)

  | BEXE_call_with_trap (sr,e1,e2) -> 
    bexe_call_with_trap (sr, ce sr e1, ce sr e2)


  | BEXE_jump (sr,(BEXPR_closure (i,ts),t),e2) -> 
    begin
      let bsym = Flx_bsym_table.find bsym_table i in
      match Flx_bsym.bbdcl bsym with
      (* tail call optimisation for a non-returning function
         can't be applied to a primitive
      *)
      | BBDCL_external_fun _ -> bexe_call_prim (sr,i,ts,ce sr e2) 
      | BBDCL_fun _  -> bexe_jump_direct (sr,i,ts, ce sr e2)
      | _ -> assert false
    end
  | BEXE_jump (sr,e1,e2) -> 
    bexe_jump (sr, ce sr e1, ce sr e2)


  | BEXE_ifgoto (sr,e,idx) -> bexe_ifgoto (sr, ce sr e,idx)
  | BEXE_cgoto (sr,e) -> bexe_cgoto (sr, ce sr e)
  | BEXE_ifcgoto (sr,e1,e2) -> bexe_ifcgoto (sr, ce sr e1, ce sr e2)
  | BEXE_fun_return (sr,e) -> bexe_fun_return (sr,ce sr e)

  | BEXE_code (sr,s,e) -> bexe_code (sr,s, ce sr e)
  | BEXE_nonreturn_code (sr,s,e) -> bexe_nonreturn_code (sr,s,ce sr e)

  | BEXE_yield (sr,e) -> bexe_yield (sr,ce sr e)

  | BEXE_init (sr,i,e) -> bexe_init (sr,i,ce sr e)
  | BEXE_assign (sr,e1,e2) -> bexe_assign (sr, ce sr e1, ce sr e2)
  | BEXE_storeat (sr,e1,e2) -> bexe_storeat (sr, ce sr e1, ce sr e2)
  | BEXE_assert (sr,e) -> bexe_assert (sr, ce sr e)
  | BEXE_axiom_check2 (sr,sr2,e1,e2) ->
    let e1 = match e1 with Some e -> Some (ce sr e) | None -> None in
    bexe_axiom_check2 (sr, sr2,e1,ce sr e2)
  | BEXE_assert2 (sr,sr2,e1,e2) ->
    let e1 = match e1 with Some e -> Some (ce sr e) | None -> None in
    bexe_assert2 (sr, sr2,e1,ce sr e2)

  | BEXE_svc (sr,i) -> exe

  | BEXE_catch _ 
  | BEXE_try _
  | BEXE_endtry  _
  | BEXE_label _
  | BEXE_halt _
  | BEXE_trace _
  | BEXE_goto _
  | BEXE_comment _
  | BEXE_nop _
  | BEXE_proc_return _
  | BEXE_begin
  | BEXE_end -> exe

  | BEXE_axiom_check _  -> assert false
  | BEXE_call_prim _  -> assert false
  | BEXE_call_direct _  -> assert false

  | BEXE_jump_direct _  -> assert false
  | BEXE_call_stack _  -> assert false


let process_exes state bsym_table nutab exes =
  List.map (chk_exe state bsym_table nutab ) exes

let process_entry state bsym_table nutab i parent bsym =
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,ps,ret,effects,exes) ->
    let exes = process_exes state bsym_table nutab exes in
    let bbdcl = bbdcl_fun (props,vs,ps,ret,effects,exes) in
    let bsym = (Flx_bsym.replace_bbdcl bsym bbdcl) in
    Flx_bsym_table.add nutab i parent bsym

  | _ -> Flx_bsym_table.add nutab i parent bsym

(* This routine lifts lambda terms and creates function wrappers for
   non-function terms passed as arguments or returned as values

  These terms are: 
    compositions
    primitive functions
    injections
    non-const union constructors
    projections
    array projections
    structs
    cstructs 
    c function pointers
*)
 
let make_wrappers syms bsym_table =
  let nutab = Flx_bsym_table.create_from bsym_table in
  let state = make_closure_state syms in 
  Flx_bsym_table.iter (process_entry state bsym_table nutab) bsym_table;
  nutab



