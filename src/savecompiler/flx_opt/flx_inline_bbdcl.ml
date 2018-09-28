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


let rec heavily_inline_bbdcl syms uses bsym_table excludes i =
  let bsym =
    try Some (Flx_bsym_table.find bsym_table i)
    with Not_found -> None
  in
  match bsym with None -> () | Some bsym ->
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,ps,ret,effects,exes) ->
    assert (vs=[]);
    if not (mem `Inlining_started props) then begin
      let props = `Inlining_started :: props in
      let bbdcl = bbdcl_fun (props,[],ps,ret,effects, exes) in
      Flx_bsym_table.update_bbdcl bsym_table i bbdcl;
      (* inline into all children first *)
      let children = Flx_bsym_table.find_children bsym_table i in
      BidSet.iter (fun i-> heavily_inline_bbdcl syms uses bsym_table excludes i) children;

(*
      let exes = check_reductions syms bsym_table exes in (* user reductions *)
*)
      let xcls = Flx_tailit.exes_get_xclosures syms exes in
      BidSet.iter (fun i-> heavily_inline_bbdcl syms uses bsym_table excludes i) xcls;

      if syms.compiler_options.print_flag then
      print_endline ("HIB:Examining function " ^ Flx_bsym.id bsym ^ "<" ^
        string_of_bid i ^ "> for inlinable calls");
      
      let exes = List.map Flx_bexe.reduce exes in (* term reduction *)
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
      let exes = fold_vars syms bsym_table uses i ps exes in
(*
      let exes = fold_vars2 syms bsym_table exes in
*)
(*
      let exes = check_reductions syms bsym_table exes in (* user reductions *)
*)
(*
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
*)
      let exes = Flx_inline_calls.heavy_inline_calls
        syms
        bsym_table
        uses
        heavily_inline_bbdcl
        i
        excludes
        exes
      in
      
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
      let exes = Flx_tailit.tailit
        syms
        bsym_table
        uses
        (Flx_bsym.id bsym)
        i
        (Flx_bsym.sr bsym)
        ps
        exes
      in
(*
      let exes = check_reductions syms bsym_table exes in (* user reductions *)
*)
      let exes = List.map Flx_bexe.reduce exes in (* term reduction *)
(*
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
*)
      let exes = fold_vars syms bsym_table uses i ps exes in
(*
      let exes = fold_vars2 syms bsym_table exes in
*)
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
(*
      let exes = check_reductions syms bsym_table exes in (* user reductions *)
*)
      let exes = Flx_cflow.chain_gotos syms (Flx_bsym.id bsym) ret exes in
(*
      let exes = List.map Flx_bexe.reduce exes in
*)
      let props = `Inlining_complete :: props in
      let bbdcl = bbdcl_fun (props,[],ps,ret,effects, exes) in
      Flx_bsym_table.update_bbdcl bsym_table i bbdcl;
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
      Flx_remove_unused_children.remove_unused_children syms uses bsym_table i;
    end
  | _ -> ()


