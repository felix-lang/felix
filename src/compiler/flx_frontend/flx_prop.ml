open Flx_util
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_mbind
open List
open Flx_unify
open Flx_treg
open Flx_exceptions
open Flx_use

let add_prop bsym_table p i =
  let id,parent,sr,entry = Hashtbl.find bsym_table i in
  match entry with
  | BBDCL_function (props,vs,ps,ret,exes) ->
    let entry = BBDCL_function (p :: props,vs,ps,ret,exes) in
    Hashtbl.replace bsym_table i (id,parent,sr,entry);

  (* because of type classes .. *)
  | BBDCL_const (props,vs,ret,ct,reqs) ->
    let entry = BBDCL_const (p :: props,vs,ret,ct,reqs) in
    Hashtbl.replace bsym_table i (id,parent,sr,entry);

  | BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) ->
    let entry = BBDCL_fun (p :: props,vs,ps,ret,ct,reqs,prec) in
    Hashtbl.replace bsym_table i (id,parent,sr,entry);

  | BBDCL_procedure (props,vs,ps,exes) ->
    let entry = BBDCL_procedure (p :: props,vs,ps,exes) in
    Hashtbl.replace bsym_table i (id,parent,sr,entry)

  (* because of type classes .. *)
  | BBDCL_proc (props,vs,ps,ct,reqs) ->
    let entry = BBDCL_proc (p :: props,vs,ps,ct,reqs) in
    Hashtbl.replace bsym_table i (id,parent,sr,entry);

  | _ -> ()

let rem_prop bsym_table p i =
  let id,parent,sr,entry = Hashtbl.find bsym_table i in
  match entry with
  | BBDCL_function (props,vs,ps,ret,exes) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = BBDCL_function (props,vs,ps,ret,exes) in
    Hashtbl.replace bsym_table i (id,parent,sr,entry);

  (* because of type classes .. *)
  | BBDCL_const (props,vs,ret,ct,reqs) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = BBDCL_const (props,vs,ret,ct,reqs) in
    Hashtbl.replace bsym_table i (id,parent,sr,entry);

  | BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) in
    Hashtbl.replace bsym_table i (id,parent,sr,entry);

  | BBDCL_procedure (props,vs,ps,exes) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = BBDCL_procedure (props,vs,ps,exes) in
    Hashtbl.replace bsym_table i (id,parent,sr,entry)

  (* because of type classes .. *)
  | BBDCL_proc (props,vs,ps,ct,reqs) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = BBDCL_proc (props,vs,ps,ct,reqs) in
    Hashtbl.replace bsym_table i (id,parent,sr,entry);

  | _ -> ()

let get_vs bsym_table i =
  let _,_,_,entry = Hashtbl.find bsym_table i in
  match entry with
  | BBDCL_function (props,vs,(ps,traint),ret,exes) -> vs
  | BBDCL_procedure (props,vs,(ps,traint), exes) -> vs
  | BBDCL_val (vs,t) -> vs
  | BBDCL_var (vs,t) -> vs
  | BBDCL_ref (vs,t) -> vs
  | BBDCL_tmp (vs,t) -> vs
  | BBDCL_union (vs,ps) -> vs
  | BBDCL_struct (vs,ps) -> vs
  | BBDCL_cstruct (vs,ps) -> vs
  | BBDCL_newtype (vs,t) -> vs
  | BBDCL_const (_,vs,t,ct,reqs) -> vs
  | BBDCL_insert (vs,s,ikind,reqs) -> vs
  | BBDCL_fun (props,vs,argtypes,ret,ct,reqs,prec) -> vs
  | BBDCL_callback (props,vs,argtypes_cf,argtypes_c,k,ret,reqs,prec) -> vs
  | BBDCL_proc (props,vs,argtypes,ct,reqs) -> vs
  | BBDCL_abs (vs,tqual,ct,reqs) ->  vs
  | BBDCL_nonconst_ctor (vs,uidx,udt, ctor_idx, ctor_argt, evs, etraint) -> vs
  | BBDCL_typeclass (props,vs) ->  vs
  | BBDCL_instance (props,vs,con,tc,ts) -> vs
