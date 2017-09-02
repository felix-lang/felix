open List
open Flx_ast
open Flx_bbdcl
open Flx_bexe
open Flx_bexpr
open Flx_btype
open Flx_exceptions
open Flx_foldvars
(* open Flx_foldvars2 *)
open Flx_list
open Flx_maps
open Flx_mtypes2
open Flx_options
open Flx_print
open Flx_reparent
open Flx_set
open Flx_spexes
open Flx_types
open Flx_typing
open Flx_unify
open Flx_use
open Flx_util
open Flx_bid

(* the function u expands an expression into an expression and some exes
which must be executed before the exe containing it, it has to
return these exes in REVERSE order because each case pushes the
base exe onto the top of the list.

The function u wraps special_inline.

At the end this function reverses the order so it returns the
expansion in FORWARD order.
*)
let expand_exe syms bsym_table u exe =
  let xs =
    (*
    print_endline ("EXPAND EXE " ^ string_of_bexe bsym_table 0 exe);
    *)
    match exe with
    | BEXE_axiom_check _ -> assert false
    | BEXE_call_prim (sr,i,ts,e2) -> 
      let e,xs = u sr e2 in
      bexe_call_prim (sr,i,ts,e) :: xs

    | BEXE_call_stack (sr,i,ts,e2) -> assert false

    | BEXE_call_direct (sr,i,ts,e2) -> 
      let e,xs = u sr e2 in
      bexe_call_direct (sr,i,ts,e) :: xs

    | BEXE_jump_direct (sr,i,ts,e2) -> 
      let e,xs = u sr e2 in
      bexe_jump_direct (sr,i,ts,e) :: xs

    | BEXE_assign (sr,e1,e2) ->
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_assign (sr,e1,e2) :: xs2 @ xs1

    | BEXE_storeat (sr,e1,e2) ->
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_storeat (sr,e1,e2) :: xs2 @ xs1

    | BEXE_assert (sr,e) ->
      let e,xs = u sr e in
      bexe_assert (sr,e) :: xs

    | BEXE_assert2 (sr,sr2,e1,e2) ->
      let e1,xs1 =
        match e1 with Some e -> let a,b = u sr e in Some a,b
        | None -> None,[]
      in
      let e2,xs2 = u sr e2 in
      bexe_assert2 (sr,sr2,e1,e2) :: xs2 @ xs1

    | BEXE_axiom_check2 (sr,sr2,e1,e2) ->
      let e1,xs1 =
        match e1 with Some e -> let a,b = u sr e in Some a,b
        | None -> None,[]
      in
      let e2,xs2 = u sr e2 in
      bexe_axiom_check2 (sr,sr2,e1,e2) :: xs2 @ xs1

    (* preserve call lift pattern ??*)
    | BEXE_call (sr,(BEXPR_apply((BEXPR_closure(i,ts),_),e1),t),e2) 
    | BEXE_call (sr,(BEXPR_apply_direct(i,ts,e1),t),e2) ->
      assert (ts=[]);
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_call (sr,
        (bexpr_apply_direct t (i,ts,e1)),
        e2) :: xs2 @ xs1

    | BEXE_call (sr,e1,e2) ->
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_call (sr,e1,e2) :: xs2 @ xs1

    | BEXE_call_with_trap (sr,e1,e2) ->
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_call_with_trap (sr,e1,e2) :: xs2 @ xs1


    | BEXE_jump (sr,e1,e2) -> 
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_jump (sr,e1,e2) :: xs2 @ xs1

    | BEXE_ifgoto (sr,e,idx) ->
      let e,xs = u sr e in
      bexe_ifgoto (sr,e,idx) :: xs

    | BEXE_cgoto (sr,e) ->
      let e,xs = u sr e in
      bexe_cgoto (sr,e) :: xs

    | BEXE_ifcgoto (sr,e1,e2) ->
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_ifcgoto (sr,e1,e2) :: xs2 @ xs1


    (* preserve tail call pattern -- used by both
       tail-rec eliminator
       and by call lifter (which converts returns to calls)
    *)
    | BEXE_fun_return (sr,(BEXPR_apply((BEXPR_closure(i,ts),_),e),t)) 
    | BEXE_fun_return (sr,(BEXPR_apply_direct(i,ts,e),t)) ->
      assert (ts=[]);
      let e,xs = u sr e in
      bexe_fun_return (sr,
        (bexpr_apply_direct t (i,ts,e))) :: xs

    | BEXE_fun_return (sr,e) ->
      let e,xs = u sr e in
      bexe_fun_return (sr,e) :: xs

    | BEXE_yield (sr,e) ->
      let e,xs = u sr e in
      bexe_yield (sr,e) :: xs

    (* This case has to be handled specially, in case we already
       have a simplified form, and the unravelling introduces
       a gratuitous extra variable: for example

       x : = f a

       might expand to

       x' = f a
       x := x'

       which is rather pointless. There is, unfortunately,
       a duplicate of this check elsewhere ..
    *)

    | BEXE_init (sr,i,(BEXPR_apply((BEXPR_closure (j,ts),_),e),t))
    | BEXE_init (sr,i,(BEXPR_apply_direct (j,ts,e),t))
      (*
      when is_generator bsym_table j
      *)
      ->
      assert (ts=[]);
      let e,xs = u sr e in
      bexe_init (sr, i,
        (bexpr_apply_direct t (j,ts,e))) :: xs

    | BEXE_init (sr,i,e) ->
      let e,xs = u sr e in
      bexe_init (sr,i,e) :: xs

    | BEXE_svc _
    | BEXE_label _
    | BEXE_goto _
    | BEXE_code _
    | BEXE_nonreturn_code _
    | BEXE_proc_return _
    | BEXE_comment _
    | BEXE_nop _
    | BEXE_halt _
    | BEXE_trace _
    | BEXE_begin
    | BEXE_end
    | BEXE_try _
    | BEXE_catch _
    | BEXE_endtry _
      -> [exe]
  in
    let xs = rev xs in
    xs


