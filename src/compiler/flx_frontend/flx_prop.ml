open Flx_util
open Flx_ast
open Flx_bbdcl
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open List
open Flx_unify
open Flx_exceptions
open Flx_use

let add_prop bsym_table p i =
  let bsym = Flx_bsym_table.find bsym_table i in
  let update_bsym bbdcl = Flx_bsym_table.update_bbdcl bsym_table i bbdcl in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_function (props,vs,ps,ret,exes) ->
      update_bsym (bbdcl_function (p :: props,vs,ps,ret,exes))

  (* because of type classes .. *)
  | BBDCL_const (props,vs,ret,ct,reqs) ->
      update_bsym (bbdcl_const (p :: props,vs,ret,ct,reqs))

  | BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) ->
      update_bsym (bbdcl_fun (p :: props,vs,ps,ret,ct,reqs,prec))

  (* because of type classes .. *)
  | BBDCL_proc (props,vs,ps,ct,reqs) ->
      update_bsym (bbdcl_proc (p :: props,vs,ps,ct,reqs))

  | _ -> ()

let rem_prop bsym_table p i =
  let bsym = Flx_bsym_table.find bsym_table i in
  let update_bsym bbdcl = Flx_bsym_table.update_bbdcl bsym_table i bbdcl in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_function (props,vs,ps,ret,exes) ->
      let props = List.filter (fun k -> p <> k) props in
      update_bsym (bbdcl_function (props,vs,ps,ret,exes))

  (* because of type classes .. *)
  | BBDCL_const (props,vs,ret,ct,reqs) ->
      let props = List.filter (fun k -> p <> k) props in
      update_bsym (bbdcl_const (props,vs,ret,ct,reqs))

  | BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) ->
      let props = List.filter (fun k -> p <> k) props in
      update_bsym (bbdcl_fun (props,vs,ps,ret,ct,reqs,prec))

  (* because of type classes .. *)
  | BBDCL_proc (props,vs,ps,ct,reqs) ->
    let props = List.filter (fun k -> p <> k) props in
    update_bsym (bbdcl_proc (props,vs,ps,ct,reqs))

  | _ -> ()
