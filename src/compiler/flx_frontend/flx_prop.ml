open Flx_util
open Flx_ast
open Flx_types
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
  let update_bsym bbdcl =
    Flx_bsym_table.add bsym_table i { bsym with Flx_bsym.bbdcl=bbdcl }
  in
  match bsym.Flx_bsym.bbdcl with
  | BBDCL_function (props,vs,ps,ret,exes) ->
    update_bsym (BBDCL_function (p :: props,vs,ps,ret,exes))

  (* because of type classes .. *)
  | BBDCL_const (props,vs,ret,ct,reqs) ->
    update_bsym (BBDCL_const (p :: props,vs,ret,ct,reqs))

  | BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) ->
    update_bsym (BBDCL_fun (p :: props,vs,ps,ret,ct,reqs,prec))

  | BBDCL_procedure (props,vs,ps,exes) ->
    update_bsym (BBDCL_procedure (p :: props,vs,ps,exes))

  (* because of type classes .. *)
  | BBDCL_proc (props,vs,ps,ct,reqs) ->
    update_bsym (BBDCL_proc (p :: props,vs,ps,ct,reqs))

  | _ -> ()

let rem_prop bsym_table p i =
  let bsym = Flx_bsym_table.find bsym_table i in
  let update_bsym bbdcl =
    Flx_bsym_table.add bsym_table i { bsym with Flx_bsym.bbdcl=bbdcl }
  in
  match bsym.Flx_bsym.bbdcl with
  | BBDCL_function (props,vs,ps,ret,exes) ->
    let props = List.filter (fun k -> p <> k) props in
    update_bsym (BBDCL_function (props,vs,ps,ret,exes))

  (* because of type classes .. *)
  | BBDCL_const (props,vs,ret,ct,reqs) ->
    let props = List.filter (fun k -> p <> k) props in
    update_bsym (BBDCL_const (props,vs,ret,ct,reqs))

  | BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) ->
    let props = List.filter (fun k -> p <> k) props in
    update_bsym (BBDCL_fun (props,vs,ps,ret,ct,reqs,prec))

  | BBDCL_procedure (props,vs,ps,exes) ->
    let props = List.filter (fun k -> p <> k) props in
    update_bsym (BBDCL_procedure (props,vs,ps,exes))

  (* because of type classes .. *)
  | BBDCL_proc (props,vs,ps,ct,reqs) ->
    let props = List.filter (fun k -> p <> k) props in
    update_bsym (BBDCL_proc (props,vs,ps,ct,reqs))

  | _ -> ()
