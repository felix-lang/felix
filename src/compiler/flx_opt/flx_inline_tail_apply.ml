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
open Flx_set
open Flx_spexes
open Flx_types
open Flx_typing
open Flx_unify
open Flx_use
open Flx_util
open Flx_bid


let inline_tail_apply syms uses bsym_table caller callee a =
  (* TEMPORARY .. this should be allowed for unrolling but we do not do that yet *)
  assert (callee <> caller);
  let bsym = Flx_bsym_table.find bsym_table callee in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,ps,ret,effects,exes) ->
    assert (vs=[]);
    (*
    let id2,_,_,_ = hfind "inline-tail[function]" bsym_table caller in
    print_endline
    (
      "TAIL Inlining function "^id^
      "<"^si callee^">"^
      "[" ^ catmap "," (sbt bsym_table) ts ^ "] into " ^ id2 ^ "<" ^ si caller ^">"
    );
    *)
    let revariable = Flx_reparent.reparent_children
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
    revariable,rev body

  | _ -> assert false


