open List

open Flx_bbdcl
open Flx_beta
open Flx_bexe
open Flx_bexpr
open Flx_bparameter
open Flx_btype
open Flx_cexpr
open Flx_ctorgen
open Flx_ctypes
open Flx_display
open Flx_egen
open Flx_exceptions
open Flx_label
open Flx_list
open Flx_maps
open Flx_mtypes2
open Flx_name
open Flx_ogen
open Flx_options
open Flx_pgen
open Flx_print
open Flx_types
open Flx_typing
open Flx_unify
open Flx_util
open Flx_gen_helper
open Flx_bid
open Flx_btype_subst

let gen_function_names syms bsym_table =
  let xxsym_table = ref [] in
  Hashtbl.iter
  (fun x i ->
    (* if proper_descendant parent then  *)
    xxsym_table := (i,x) :: !xxsym_table
  )
  syms.instances
  ;

  let s = Buffer.create 2000 in
  List.iter
  (fun (i,(index,ts)) ->
    let tss =
      if length ts = 0 then "" else
      "[" ^ catmap "," (sbt bsym_table) ts^ "]"
    in
    let bsym =
      try Flx_bsym_table.find bsym_table index with Not_found ->
        failwith ("[gen_functions] can't find index " ^ string_of_bid index)
    in
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,(ps,traint), _, effects,_) ->
      if mem `Cfun props || mem `Pure props && not (mem `Heap_closure props) then begin
      end else begin
        let name = cpp_instance_name syms bsym_table index ts in
        bcat s ("struct " ^ name ^ ";\n");
      end

    | _ -> () (* bcat s ("//SKIPPING " ^ id ^ "\n") *)
  )
  (sort compare !xxsym_table)
  ;
  Buffer.contents s



