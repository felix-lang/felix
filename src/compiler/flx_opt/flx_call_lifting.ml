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
(* CALL LIFTING. What this does is transform a call:

  call (f a) arg

  by replacing it with the body of f,
  in which every

  return x

  is replaced by

  call x arguemnt

  This converts  f from a function returning
  a procedure, to a procedure which executes that
  procedure.

  NOTE: this is a special case of the distributive law.

  f (if c then a else b) v => if c then f a v else f b v

*)
let call_lifting syms uses bsym_table caller callee a argument =
  (*
  print_endline "DOING CALL LIFTING";
  *)

  (* Get the callee from the symbol binding table *)
  let bsym = Flx_bsym_table.find bsym_table callee in

  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,ps,ret,effects,exes) ->

    (* Ensure that we aren't dealing with any type variables. *)
    assert (vs=[]);

    (*
    print_endline ("Found procedure "^id^": Inline it!");
    *)
    let revariable = reparent_children
      syms uses bsym_table
      callee (Some caller) false []
    in

    (* use the inliner to handle the heavy work *)
    let body = gen_body
      syms
      uses bsym_table
      (Flx_bsym.id bsym)
      ps
      revariable
      exes
      a
      (Flx_bsym.sr bsym)
      caller
      callee
      `Lazy
      props
    in

    (* replace all function returns with tailed calls *)
    let body2 = ref [] in
    let end_index = fresh_bid syms.counter in
    let end_label = "_end_call_lift_" ^ string_of_bid end_index in
    let bbdcl = Flx_bbdcl.bbdcl_label end_label in
    let bsym = {Flx_bsym.id=end_label; sr=Flx_bsym.sr bsym; bbdcl=bbdcl} in 
    let parent = Some caller in 
(*
print_endline ("flx_inline: call lifting: adding label " ^ end_label ^ "<" ^ string_of_int end_index ^">");
*)
    Flx_bsym_table.add bsym_table end_index parent bsym;

    (* Got too lazy to tack if this is used or not! *) 
    body2 := bexe_label (Flx_bsym.sr bsym,end_index) :: !body2;

    List.iter
      (function
      | BEXE_fun_return (sr,e) ->

        (* NOTE REVERSED ORDER *)
        let call_instr =
          (
          match e with
          | BEXPR_closure (i,ts),_ ->
            assert (ts=[]);
            bexe_call_direct (sr,i,ts,argument)
          | _ ->
            bexe_call (sr,e,argument)
          )
        in
        body2 := bexe_goto (sr,end_index) :: !body2;
        body2 := call_instr :: !body2;
      | BEXE_yield _ ->
        syserr (Flx_bsym.sr bsym) "Attempt to inline generator containing a yield"
      | x -> body2 := x::!body2
      )
      body
    ;
    (*
    print_endline (
     catmap "\n" (string_of_bexe bsym_table 0) !body2
    )
    ;
    *)
    revariable,!body2 (* forward order *)

  | _ -> assert false


