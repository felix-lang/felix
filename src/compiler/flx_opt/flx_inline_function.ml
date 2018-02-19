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

let inline_function syms uses bsym_table caller callee a varindex =
  (*
  print_endline ("Inline function: init var index " ^ si varindex);
  *)
  let bsym = Flx_bsym_table.find bsym_table callee in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,ps,ret,effects,exes) ->
    (*
    print_endline
    (
      "Inlining function "^id^
      "<"^si callee^">"^
      "[" ^ catmap "," (sbt bsym_table) ts ^ "]"^
      " retvar="^ si varindex ^
      "\nvs = " ^ catmap "," (fun (s,i) -> s ^ "<" ^ si i ^ ">") vs
    );
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

    (*
    print_endline "Replace returns with inits";
    *)
    (* replace all function returns with variable initialisations *)
    let body2 = ref [] in
    let end_index = fresh_bid syms.counter in
    let end_label = "_end_inline_" ^ Flx_bsym.id bsym ^ "_" ^ string_of_bid end_index in
    let t = ref None in
    let end_label_used = ref false in
    List.iter
      (function
      | BEXE_fun_return (sr,((_,t') as e)) ->
        t := Some t';
        if not (!body2 == []) then begin
          body2 := bexe_goto (sr,end_index) :: !body2;
          end_label_used := true
        end
        ;
        let call_instr = bexe_init (sr,varindex,e) in
        (*
        print_endline ("Replacing return with init: " ^ string_of_bexe bsym_table 0 call_instr);
        *)
        body2 := call_instr :: !body2;

      | BEXE_yield _ ->
        syserr (Flx_bsym.sr bsym) "Attempt to inline generator with a yield"

      | x -> body2 := x::!body2
      )
      body
    ;
    (* Ugghhh *)
    if !end_label_used then begin
      let bbdcl = Flx_bbdcl.bbdcl_label end_label in
      let bsym = {Flx_bsym.id=end_label; sr=Flx_bsym.sr bsym; bbdcl=bbdcl} in 
      let parent = Some caller in 
(*
print_endline ("flx_inline: inline function : adding label " ^ end_label ^ "<" ^ string_of_int end_index ^">");
*)
      Flx_bsym_table.add bsym_table end_index parent bsym;
      body2 := !body2 @ [bexe_label (Flx_bsym.sr bsym,end_index)]
    end
    ;
    (*
    print_endline (
     catmap "\n" (string_of_bexe bsym_table 0) !body2
    )
    ;
    *)
    revariable,!body2 (* forward order *)

  | _ -> assert false


