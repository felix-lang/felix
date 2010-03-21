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

type closure_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  wrappers : (Flx_types.bid_t, Flx_types.bid_t) Hashtbl.t;
}

let make_closure_state syms =
  {
    syms = syms;
    wrappers = Hashtbl.create 97;
  }

(** This generates closures for calling external functions. It does this by
 * generating a new function that contains part of the closed values. *)
let gen_closure state bsym_table bid t =
  let bsym = Flx_bsym_table.find bsym_table bid in
  let bsym_parent = Flx_bsym_table.find_parent bsym_table bid in

  (* Make a bid for our closure wrapper function. *)
  let closure_bid = fresh_bid state.syms.counter in

  (* Add the closure wrapper to symbol table. We'll replace it later with the
   * real values. *)
  Flx_bsym_table.add bsym_table bsym_parent closure_bid (Flx_bsym.create
    ~sr:(Flx_bsym.sr bsym)
    ("_a" ^ string_of_int closure_bid ^ "_" ^ Flx_bsym.id bsym)
    (bbdcl_invalid ()));

  let make_wrapped_call vs ps =
    (* Make the type of the closed value. *)
    let closed_type = match ps with [t] -> t | ts -> btyp_tuple ts in

    (* Make the closed value that's hidden inside our wrapper function. *)
    let closed_bid = fresh_bid state.syms.counter in
    let closed_name = "_a" ^ string_of_bid closed_bid in
    let closed_val = bbdcl_val (vs,closed_type) in

    Flx_bsym_table.add bsym_table (Some closure_bid) closed_bid
      (Flx_bsym.create ~sr:(Flx_bsym.sr bsym) closed_name closed_val);

    (* Make the parameters for the wrapper function. *)
    let ps =
      [{ pkind=`PVal;
         pid=closed_name;
         pindex=closed_bid;
         ptyp=closed_type } ]
    in

    (* Make the type variables of the inner call. *)
    let ts = List.map (fun (_,i) -> btyp_type_var (i, btyp_type 0)) vs in

    (* Make the argument that we'll pass to our wrapped function. *)
    let arg = bexpr_name closed_type (closed_bid, ts) in

    (* Return a couple parameters *)
    ts, ps, arg
  in

  let bbdcl =
    match Flx_bsym.bbdcl bsym with
    | BBDCL_proc (_,vs,ps,_,_) ->
        let ts, ps, arg = make_wrapped_call vs ps in

        (* Generate a call to the wrapped procedure. *)
        let exes =
          [ bexe_call_prim (Flx_bsym.sr bsym, bid, ts, arg);
            bexe_proc_return (Flx_bsym.sr bsym) ]
        in

        bbdcl_procedure ([],vs,(ps,None),exes)

    | BBDCL_fun (_,vs,ps,ret,_,_,_) ->
        let ts, ps, arg = make_wrapped_call vs ps in

        (* Generate a call to the wrapped function. *)
        let e = bexpr_apply_prim ret (bid, ts, arg) in
        let exes = [bexe_fun_return (Flx_bsym.sr bsym, e)] in

        bbdcl_function ([],vs,(ps,None),ret,exes)

    | BBDCL_struct (vs,ps)
    | BBDCL_cstruct (vs,ps) ->
        let ts, params, arg = make_wrapped_call vs (List.map snd ps) in

        (* Generate a call to the wrapped function. *)
        let e = bexpr_apply_struct t (bid, ts, arg) in
        let exes = [bexe_fun_return (Flx_bsym.sr bsym, e)] in

        bbdcl_function ([],vs,(params,None),btyp_inst (bid,[]),exes)

    | _ -> assert false
  in

  (* Finally, replace our wrapper temp bbdcl with our new bbdcl. *)
  Flx_bsym_table.update_bbdcl bsym_table closure_bid bbdcl;

  (* Return the wrapper. *)
  closure_bid

let mkcls state bsym_table all_closures i ts t =
  let j =
    try Hashtbl.find state.wrappers i
    with Not_found ->
      let j = gen_closure state bsym_table i t in
      Hashtbl.add state.wrappers i j;
      j
  in
    all_closures := BidSet.add j !all_closures;
    bexpr_closure t (j,ts)

let check_prim state bsym_table all_closures i ts t =
  match Flx_bsym_table.find_bbdcl bsym_table i with
  | BBDCL_proc _
  | BBDCL_fun _
  | BBDCL_struct _
  | BBDCL_cstruct _ ->
      mkcls state bsym_table all_closures i ts t

  | x ->
      all_closures := BidSet.add i !all_closures;
      bexpr_closure t (i,ts)

let idt t = t

let ident x = x

let rec adj_cls state bsym_table all_closures e =
  let adj e = adj_cls state bsym_table all_closures e in
  match Flx_bexpr.map ~f_bexpr:adj e with
  | BEXPR_closure (i,ts),t ->
    check_prim state bsym_table all_closures i ts t

  (* Direct calls to non-stacked functions require heap
     but not a clone ..
  *)
  | BEXPR_apply_direct (i,ts,a),t as x ->
    all_closures := BidSet.add i !all_closures;
    x

  | x -> x


let process_exe state bsym_table all_closures exe =
  let ue e = adj_cls state bsym_table all_closures e in
  match exe with
  | BEXE_axiom_check _ -> assert false
  | BEXE_call_prim (sr,i,ts,e2) -> bexe_call_prim (sr,i,ts, ue e2)

  | BEXE_call_direct (sr,i,ts,e2) ->
    all_closures := BidSet.add i !all_closures;
    bexe_call_direct (sr,i,ts, ue e2)

  | BEXE_jump_direct (sr,i,ts,e2)  ->
    all_closures := BidSet.add i !all_closures;
    bexe_jump_direct (sr,i,ts, ue e2)

  | BEXE_call_stack (sr,i,ts,e2)  ->
    (* stack calls do use closures -- but not heap allocated ones *)
    bexe_call_stack (sr,i,ts, ue e2)

  | BEXE_call (sr,e1,e2) -> bexe_call (sr,ue e1, ue e2)
  | BEXE_jump (sr,e1,e2) -> bexe_jump (sr,ue e1, ue e2)

  | BEXE_ifgoto (sr,e,l) -> bexe_ifgoto (sr, ue e,l)
  | BEXE_fun_return (sr,e) -> bexe_fun_return (sr,ue e)
  | BEXE_yield (sr,e) -> bexe_yield (sr,ue e)

  | BEXE_init (sr,i,e) -> bexe_init (sr,i,ue e)
  | BEXE_assign (sr,e1,e2) -> bexe_assign (sr, ue e1, ue e2)
  | BEXE_assert (sr,e) -> bexe_assert (sr, ue e)
  | BEXE_assert2 (sr,sr2,e1,e2) ->
    let e1 = match e1 with Some e -> Some (ue e) | None -> None in
    bexe_assert2 (sr, sr2,e1,ue e2)

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

let process_exes state bsym_table all_closures exes =
  List.map (process_exe state bsym_table all_closures) exes

let process_entry state bsym_table all_closures i =
  let ue e = adj_cls state bsym_table all_closures e in
  let bsym = Flx_bsym_table.find bsym_table i in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_function (props,vs,ps,ret,exes) ->
    let exes = process_exes state bsym_table all_closures exes in
    let bbdcl = bbdcl_function (props,vs,ps,ret,exes) in
    Flx_bsym_table.update_bbdcl bsym_table i bbdcl

  | BBDCL_procedure (props,vs,ps,exes) ->
    let exes = process_exes state bsym_table all_closures exes in
    let bbdcl = bbdcl_procedure (props,vs,ps,exes) in
    Flx_bsym_table.update_bbdcl bsym_table i bbdcl

  | _ -> ()

(* NOTE: before monomorphisation, we can't tell if a
  typeclass method will dispatch to a C function
  or a Felix function .. so we have to mark all typeclass
  methods and probably instances as requiring a closure ..

  This is overkill and will defeat some optimisations ..
  needs to be fixed. .. Ouch .. this is too late,
  enstack has already run .. won't affect enstack.
*)

let set_closure bsym_table i = add_prop bsym_table `Heap_closure i

let make_closure state bsym_table bsyms =
  let all_closures = ref BidSet.empty in
  let used = full_use_closure_for_symbols state.syms bsym_table bsyms in
  BidSet.iter (process_entry state bsym_table all_closures) used;
  BidSet.iter (set_closure bsym_table) !all_closures;

  bsyms

let make_closures state bsym_table =
  (*
  let used = ref BidSet.empty in
  let uses i = Flx_use.uses state.syms used bsym_table true i in
  BidSet.iter uses !(state.syms.roots);
  *)

  let all_closures = ref BidSet.empty in
  let used = full_use_closure state.syms bsym_table in
  BidSet.iter (process_entry state bsym_table all_closures) used;

  (*
  (* this is a hack! *)
  Hashtbl.iter
  ( fun i entries ->
    iter (fun (vs,con,ts,j) ->
    set_closure bsym_table i;
    process_entry state.syms bsym_table all_closures j;

    (*
    set_closure bsym_table j;
    *)
    )
    entries
  )
  state.syms.typeclass_to_instance
  ;
  *)

  (*
  BidSet.iter (set_closure bsym_table `Heap_closure) (BidSet.union !all_closures !(syms.roots));
  *)

  (* Now root proc might not need a closure .. since it can be
     executed all at once
  *)
  BidSet.iter (set_closure bsym_table) !all_closures
