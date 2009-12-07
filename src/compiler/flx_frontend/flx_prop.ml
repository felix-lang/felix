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
  let id,parent,sr,entry = Flx_bsym_table.find bsym_table i in
  match entry with
  | BBDCL_function (props,vs,ps,ret,exes) ->
    let entry = BBDCL_function (p :: props,vs,ps,ret,exes) in
    Flx_bsym_table.add bsym_table i (id,parent,sr,entry);

  (* because of type classes .. *)
  | BBDCL_const (props,vs,ret,ct,reqs) ->
    let entry = BBDCL_const (p :: props,vs,ret,ct,reqs) in
    Flx_bsym_table.add bsym_table i (id,parent,sr,entry);

  | BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) ->
    let entry = BBDCL_fun (p :: props,vs,ps,ret,ct,reqs,prec) in
    Flx_bsym_table.add bsym_table i (id,parent,sr,entry);

  | BBDCL_procedure (props,vs,ps,exes) ->
    let entry = BBDCL_procedure (p :: props,vs,ps,exes) in
    Flx_bsym_table.add bsym_table i (id,parent,sr,entry)

  (* because of type classes .. *)
  | BBDCL_proc (props,vs,ps,ct,reqs) ->
    let entry = BBDCL_proc (p :: props,vs,ps,ct,reqs) in
    Flx_bsym_table.add bsym_table i (id,parent,sr,entry);

  | _ -> ()

let rem_prop bsym_table p i =
  let id,parent,sr,entry = Flx_bsym_table.find bsym_table i in
  match entry with
  | BBDCL_function (props,vs,ps,ret,exes) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = BBDCL_function (props,vs,ps,ret,exes) in
    Flx_bsym_table.add bsym_table i (id,parent,sr,entry);

  (* because of type classes .. *)
  | BBDCL_const (props,vs,ret,ct,reqs) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = BBDCL_const (props,vs,ret,ct,reqs) in
    Flx_bsym_table.add bsym_table i (id,parent,sr,entry);

  | BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) in
    Flx_bsym_table.add bsym_table i (id,parent,sr,entry);

  | BBDCL_procedure (props,vs,ps,exes) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = BBDCL_procedure (props,vs,ps,exes) in
    Flx_bsym_table.add bsym_table i (id,parent,sr,entry)

  (* because of type classes .. *)
  | BBDCL_proc (props,vs,ps,ct,reqs) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = BBDCL_proc (props,vs,ps,ct,reqs) in
    Flx_bsym_table.add bsym_table i (id,parent,sr,entry);

  | _ -> ()
