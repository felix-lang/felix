
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
open Flx_bsym
open Flx_bid

let rec add_cls bsym_table all_closures i =
  all_closures := BidSet.add i !all_closures;
  let parent = Flx_bsym_table.find_parent bsym_table i in
  match parent with
  | None -> ()
  | Some j -> if j <> 0 then add_cls bsym_table all_closures j


(* processes closures *)
let cls syms bsym_table all_closures sr e =
  match e with
  | BEXPR_closure (i,ts),t as x -> 
(*
print_endline ("mkcls  closure " ^ string_of_int i);
*)
    add_cls bsym_table all_closures i

  | BEXPR_apply_direct (i,ts,a),t as x -> 
(*
print_endline ("mkcls2  apply direct " ^ string_of_int i);
*)
    add_cls bsym_table all_closures i
   (* Direct calls to non-stacked functions require heap but not a clone. *)

  | x -> ()

let adj_cls syms bsym_table all_closures sr e =
  Flx_bexpr.iter ~f_bexpr:(cls syms bsym_table all_closures sr)  e


let process_exe ue syms bsym_table all_closures exe =
  let ue sr e = adj_cls syms bsym_table all_closures sr e in
  match exe with
  | BEXE_axiom_check _ -> assert false
  | BEXE_call_prim (sr,i,ts,e2) -> ue sr e2

  | BEXE_call_direct (sr,i,ts,e2) -> 
    add_cls bsym_table all_closures i;
    ue sr e2

  | BEXE_jump_direct (sr,i,ts,e2)  ->
    add_cls bsym_table all_closures i;
    ue sr e2

  | BEXE_call_stack (sr,i,ts,e2)  ->
    (* stack calls do use closures -- but not heap allocated ones *)
    ue sr e2

  | BEXE_call (sr,e1,e2) -> ue sr e1; ue sr e2
  | BEXE_call_with_trap (sr,e1,e2) -> ue sr e1; ue sr e2
  | BEXE_jump (sr,e1,e2) -> ue sr e1; ue sr e2

  | BEXE_ifgoto (sr,e,idx) -> ue sr e
  | BEXE_cgoto (sr,e) -> ue sr e
  | BEXE_ifcgoto (sr,e1,e2) -> ue sr e1; ue sr e2
  | BEXE_fun_return (sr,e) -> ue sr e
  | BEXE_yield (sr,e) -> ue sr e

  | BEXE_init (sr,i,e) -> ue sr e
  | BEXE_code (sr,s,e) -> ue sr e
  | BEXE_nonreturn_code (sr,s,e) -> ue sr e
  | BEXE_assign (sr,e1,e2) -> ue sr e1; ue sr e2
  | BEXE_storeat (sr,e1,e2) -> ue sr e1; ue sr e2
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
  | BEXE_comment _
  | BEXE_nop _
  | BEXE_proc_return _
  | BEXE_begin
  | BEXE_end
    -> ()

let process_exes ue syms bsym_table all_closures exes =
  List.iter (process_exe ue syms bsym_table all_closures) exes

let process_entry ue syms bsym_table all_closures i bsym=
  match bsym.bbdcl with
  | BBDCL_fun (props,vs,ps,ret,effects,exes) ->
(*
print_endline ("Process " ^ Flx_bsym.id bsym);
*)
    process_exes ue syms bsym_table all_closures exes

  | _ -> ()

let set_closure bsym_table i = add_prop bsym_table `Heap_closure i

let mark_heap_closures syms bsym_table =
(*
print_endline ("Calculating heap closures");
*)
  let all_closures = ref BidSet.empty in
  Flx_bsym_table.iter 
   (fun i _ bsym -> process_entry adj_cls syms bsym_table all_closures i bsym) 
    bsym_table
  ;
  BidSet.iter (set_closure bsym_table) !all_closures



