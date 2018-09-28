open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open List
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_reparent
open Flx_spexes
open Flx_foldvars

let unpack syms bsym_table ps a =
  List.map 
  (fun (_,prj) -> match prj with 
    | None -> a 
    | Some prj -> match prj with 
      | _,Flx_btype.BTYP_function (_,c) -> bexpr_apply c (prj,a)
      | _ -> assert false
  ) 
  (Flx_bparams.get_prjs ps) 

(* used by uncurry *)
let merge_args syms bsym_table f c a b =
  let psf = Flx_bsym_table.find_bparams bsym_table f in
  let psc = Flx_bsym_table.find_bparams bsym_table c in
  let args = unpack syms bsym_table psf a @ unpack syms bsym_table psc b in
  bexpr_tuple (btyp_tuple (map snd args)) args

(* used by mkproc *)
let append_args syms bsym_table f a b =
  let psf = Flx_bsym_table.find_bparams bsym_table f in
  let args = unpack syms bsym_table psf a @ b in
  bexpr_tuple (btyp_tuple (map snd args)) args

