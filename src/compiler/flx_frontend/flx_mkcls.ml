open Flx_util
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_mbind
open Flx_unify
open Flx_treg
open Flx_exceptions
open Flx_use
open Flx_prop

type closure_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  bbdfns: Flx_types.fully_bound_symbol_table_t;
  wrappers : (Flx_types.bid_t, Flx_types.bid_t) Hashtbl.t;
}

let make_closure_state syms bbdfns =
  {
    syms = syms;
    bbdfns = bbdfns;
    wrappers = Hashtbl.create 97;
  }

let gen_closure state i =
  let j = !(state.syms.counter) in incr state.syms.counter;
  let id,parent,sr,entry = Hashtbl.find state.bbdfns i in
  match entry with
  | BBDCL_proc (props,vs,ps,c,reqs) ->
    let arg_t =
      match ps with | [t] -> t | ps -> BTYP_tuple ps
    in
    let ts = List.map (fun (_,i) -> BTYP_var (i,BTYP_type 0)) vs in
    let ps,a =
      let n = !(state.syms.counter) in incr state.syms.counter;
      let name = "_a" ^ si n in
      let ventry = BBDCL_val (vs,arg_t) in
      Hashtbl.add state.bbdfns n (name,Some j,sr,ventry);
      [{pkind=`PVal; pid=name; pindex=n; ptyp=arg_t}],(BEXPR_name (n,ts),arg_t)
    in

    let exes : bexe_t list =
      [
        BEXE_call_prim (sr,i,ts,a);
        BEXE_proc_return sr
      ]
    in
    let entry = BBDCL_procedure ([],vs,(ps,None),exes) in
    Hashtbl.add state.bbdfns j (id,parent,sr,entry);
    j

  | BBDCL_fun (props,vs,ps,ret,c,reqs,_) ->
    let ts = List.map (fun (_,i) -> BTYP_var (i,BTYP_type 0)) vs in
    let arg_t =
      match ps with | [t] -> t | ps -> BTYP_tuple ps
    in
    let ps,a =
      let n = !(state.syms.counter) in incr state.syms.counter;
      let name = "_a" ^ si n in
      let ventry = BBDCL_val (vs,arg_t) in
      Hashtbl.add state.bbdfns n (name,Some j,sr,ventry);
      [{pkind=`PVal; pid=name; pindex=n; ptyp=arg_t}],(BEXPR_name (n,ts),arg_t)
    in
    let e = BEXPR_apply_prim (i,ts,a),ret in
    let exes : bexe_t list = [BEXE_fun_return (sr,e)] in
    let entry = BBDCL_function ([],vs,(ps,None),ret,exes) in
    Hashtbl.add state.bbdfns j (id,parent,sr,entry);
    j

  | _ -> assert false


let mkcls state all_closures i ts =
  let j =
    try Hashtbl.find state.wrappers i
    with Not_found ->
      let j = gen_closure state i in
      Hashtbl.add state.wrappers i j;
      j
  in
    all_closures := IntSet.add j !all_closures;
    BEXPR_closure (j,ts)

let check_prim state all_closures i ts =
  let _,_,_,entry = Hashtbl.find state.bbdfns i in
  match entry with
  | BBDCL_proc _
  | BBDCL_fun _ ->
    mkcls state all_closures i ts
  | _ ->
    all_closures := IntSet.add i !all_closures;
    BEXPR_closure (i,ts)

let idt t = t

let ident x = x

let rec adj_cls state all_closures e =
  let adj e = adj_cls state all_closures e in
  match Flx_maps.map_tbexpr ident adj idt e with
  | BEXPR_closure (i,ts),t ->
    check_prim state all_closures i ts,t

  (* Direct calls to non-stacked functions require heap
     but not a clone ..
  *)
  | BEXPR_apply_direct (i,ts,a),t as x ->
    all_closures := IntSet.add i !all_closures;
    x

  | x -> x


let process_exe state all_closures (exe : bexe_t) : bexe_t =
  let ue e = adj_cls state all_closures e in
  match exe with
  | BEXE_axiom_check _ -> assert false
  | BEXE_call_prim (sr,i,ts,e2)  -> BEXE_call_prim (sr,i,ts, ue e2)

  | BEXE_call_direct (sr,i,ts,e2)  ->
    all_closures := IntSet.add i !all_closures;
    BEXE_call_direct (sr,i,ts, ue e2)

  | BEXE_jump_direct (sr,i,ts,e2)  ->
    all_closures := IntSet.add i !all_closures;
    BEXE_jump_direct (sr,i,ts, ue e2)

  | BEXE_call_stack (sr,i,ts,e2)  ->
    (* stack calls do use closures -- but not heap allocated ones *)
    BEXE_call_stack (sr,i,ts, ue e2)

  | BEXE_call (sr,e1,e2)  -> BEXE_call (sr,ue e1, ue e2)
  | BEXE_jump (sr,e1,e2)  -> BEXE_jump (sr,ue e1, ue e2)

  | BEXE_ifgoto (sr,e,l) -> BEXE_ifgoto (sr, ue e,l)
  | BEXE_fun_return (sr,e) -> BEXE_fun_return (sr,ue e)
  | BEXE_yield (sr,e) -> BEXE_yield (sr,ue e)

  | BEXE_init (sr,i,e) -> BEXE_init (sr,i,ue e)
  | BEXE_assign (sr,e1,e2) -> BEXE_assign (sr, ue e1, ue e2)
  | BEXE_assert (sr,e) -> BEXE_assert (sr, ue e)
  | BEXE_assert2 (sr,sr2,e1,e2) ->
    let e1 = match e1 with Some e -> Some (ue e) | None -> None in
    BEXE_assert2 (sr, sr2,e1,ue e2)

  | BEXE_svc (sr,i) -> exe

  | BEXE_label _
  | BEXE_halt _
  | BEXE_trace _
  | BEXE_goto _
  | BEXE_code _
  | BEXE_nonreturn_code _
  | BEXE_comment _
  | BEXE_nop _
  | BEXE_proc_return _
  | BEXE_begin
  | BEXE_end
    -> exe

let process_exes state all_closures exes =
  List.map (process_exe state all_closures) exes

let process_entry state all_closures i =
  let ue e = adj_cls state all_closures e in
  let id,parent,sr,entry = Hashtbl.find state.bbdfns i in
  match entry with
  | BBDCL_function (props,vs,ps,ret,exes) ->
    let exes = process_exes state all_closures exes in
    let entry = BBDCL_function (props,vs,ps,ret,exes) in
    Hashtbl.replace state.bbdfns i (id,parent,sr,entry)

  | BBDCL_procedure (props,vs,ps,exes) ->
    let exes = process_exes state all_closures exes in
    let entry = BBDCL_procedure (props,vs,ps,exes) in
    Hashtbl.replace state.bbdfns i (id,parent,sr,entry)

  | _ -> ()

(* NOTE: before monomorphisation, we can't tell if a
  typeclass method will dispatch to a C function
  or a Felix function .. so we have to mark all typeclass
  methods and probably instances as requiring a closure ..

  This is overkill and will defeat some optimisations ..
  needs to be fixed. .. Ouch .. this is too late,
  enstack has already run .. won't affect enstack.
*)

let set_closure bbdfns i = add_prop bbdfns `Heap_closure i

let make_closures state =
  (*
  let used = ref IntSet.empty in
  let uses i = Flx_use.uses state.syms used bbdfns true i in
  IntSet.iter uses !(state.syms.roots);
  *)

  let all_closures = ref IntSet.empty in
  let used = full_use_closure state.syms state.bbdfns in
  IntSet.iter (process_entry state all_closures) used;

  (*
  (* this is a hack! *)
  Hashtbl.iter
  ( fun i entries ->
    iter (fun (vs,con,ts,j) ->
    set_closure state.bbdfns i;
    process_entry state.syms state.bbdfns all_closures j;

    (*
    set_closure state.bbdfns j;
    *)
    )
    entries
  )
  state.syms.typeclass_to_instance
  ;
  *)

  (*
  IntSet.iter (set_closure state.bbdfns `Heap_closure) (IntSet.union !all_closures !(syms.roots));
  *)

  (* Now root proc might not need a closure .. since it can be
     executed all at once
  *)
  IntSet.iter (set_closure state.bbdfns) !all_closures
