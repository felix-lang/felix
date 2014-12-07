
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

(* processes closures *)
let cls syms bsym_table all_closures sr e =
  match e with
  | BEXPR_closure (i,ts),t as x ->
   all_closures := BidSet.add i !all_closures

  | BEXPR_apply_direct (i,ts,a),t as x ->
   (* Direct calls to non-stacked functions require heap but not a clone. *)
   all_closures := BidSet.add i !all_closures

  | x -> ()

let adj_cls syms bsym_table all_closures sr e =
  Flx_bexpr.iter ~f_bexpr:(cls syms bsym_table all_closures sr)  e


let process_exe ue syms bsym_table all_closures exe =
  let ue sr e = adj_cls syms bsym_table all_closures sr e in
  match exe with
  | BEXE_axiom_check _ -> assert false
  | BEXE_call_prim (sr,i,ts,e2) -> ue sr e2

  | BEXE_call_direct (sr,i,ts,e2) ->
    all_closures := BidSet.add i !all_closures;
    ue sr e2

  | BEXE_jump_direct (sr,i,ts,e2)  ->
    all_closures := BidSet.add i !all_closures;
    ue sr e2

  | BEXE_call_stack (sr,i,ts,e2)  ->
    (* stack calls do use closures -- but not heap allocated ones *)
    ue sr e2

  | BEXE_call (sr,e1,e2) -> ue sr e1; ue sr e2
  | BEXE_jump (sr,e1,e2) -> ue sr e1; ue sr e2

  | BEXE_ifgoto (sr,e,l) -> ue sr e
  | BEXE_cgoto (sr,e) -> ue sr e
  | BEXE_fun_return (sr,e) -> ue sr e
  | BEXE_yield (sr,e) -> ue sr e

  | BEXE_init (sr,i,e) -> ue sr e
  | BEXE_assign (sr,e1,e2) -> ue sr e1; ue sr e2
  | BEXE_assert (sr,e) -> ue sr e
  | BEXE_axiom_check2 (sr,sr2,e1,e2) ->
    (match e1 with Some e -> ue sr e | None -> ());
    ue sr e2

  | BEXE_assert2 (sr,sr2,e1,e2) ->
    (match e1 with Some e -> ue sr e | None -> ());
    ue sr e2

  | BEXE_svc (sr,i) -> ()

  | BEXE_catch _ 
  | BEXE_try _
  | BEXE_endtry  _
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
    -> ()

let process_exes ue syms bsym_table all_closures exes =
  List.iter (process_exe ue syms bsym_table all_closures) exes

let process_entry ue syms bsym_table all_closures i bbdcl =
  match bbdcl with
  | BBDCL_fun (props,vs,ps,ret,exes) ->
    process_exes ue syms bsym_table all_closures exes

  | _ -> ()

let set_closure bsym_table i = add_prop bsym_table `Heap_closure i

let mark_heap_closures syms bsym_table =
  let all_closures = ref BidSet.empty in
  Flx_bsym_table.iter 
   (fun i _ bsym -> process_entry adj_cls syms bsym_table all_closures i bsym.bbdcl) 
    bsym_table
  ;
  BidSet.iter (set_closure bsym_table) !all_closures


