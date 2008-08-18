open Flx_util
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_mbind
open Flx_srcref
open List
open Flx_unify
open Flx_treg
open Flx_exceptions
open Flx_use

let add_prop bbdfns p i =
  let id,parent,sr,entry = Hashtbl.find bbdfns i in
  match entry with
  | `BBDCL_function (props,vs,ps,ret,exes) ->
    let entry = `BBDCL_function (p :: props,vs,ps,ret,exes) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry);

  (* because of type classes .. *)
  | `BBDCL_const (props,vs,ret,ct,reqs) ->
    let entry = `BBDCL_const (p :: props,vs,ret,ct,reqs) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry);

  | `BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) ->
    let entry = `BBDCL_fun (p :: props,vs,ps,ret,ct,reqs,prec) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry);

  | `BBDCL_procedure (props,vs,ps,exes) ->
    let entry = `BBDCL_procedure (p :: props,vs,ps,exes) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry)

  (* because of type classes .. *)
  | `BBDCL_proc (props,vs,ps,ct,reqs) ->
    let entry = `BBDCL_proc (p :: props,vs,ps,ct,reqs) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry);

  | `BBDCL_regmatch (props,vs,ps,t,x) ->
    let entry = `BBDCL_regmatch (p :: props, vs, ps, t, x) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry)

  | `BBDCL_reglex (props,vs,ps,le,t,x) ->
    let entry = `BBDCL_reglex (p :: props, vs, ps, le, t, x) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry)

  | `BBDCL_glr (props, vs, t, x) ->
    let entry = `BBDCL_glr (p :: props, vs, t, x) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry)

  | _ -> ()

let rem_prop bbdfns p i =
  let id,parent,sr,entry = Hashtbl.find bbdfns i in
  match entry with
  | `BBDCL_function (props,vs,ps,ret,exes) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = `BBDCL_function (props,vs,ps,ret,exes) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry);

  (* because of type classes .. *)
  | `BBDCL_const (props,vs,ret,ct,reqs) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = `BBDCL_const (props,vs,ret,ct,reqs) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry);

  | `BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = `BBDCL_fun (props,vs,ps,ret,ct,reqs,prec) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry);

  | `BBDCL_procedure (props,vs,ps,exes) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = `BBDCL_procedure (props,vs,ps,exes) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry)

  (* because of type classes .. *)
  | `BBDCL_proc (props,vs,ps,ct,reqs) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = `BBDCL_proc (props,vs,ps,ct,reqs) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry);

  | `BBDCL_regmatch (props,vs,ps,t,x) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = `BBDCL_regmatch (props, vs, ps, t, x) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry)

  | `BBDCL_reglex (props,vs,ps,le,t,x) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = `BBDCL_reglex (props, vs, ps, le, t, x) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry)

  | `BBDCL_glr (props, vs, t, x) ->
    let props = List.filter (fun k -> p <> k) props in
    let entry = `BBDCL_glr (props, vs, t, x) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry)

  | _ -> ()

let get_vs bbdfns i =
  let _,_,_,entry = Hashtbl.find bbdfns i in
  match entry with
  | `BBDCL_function (props,vs,(ps,traint),ret,exes) -> vs
  | `BBDCL_procedure (props,vs,(ps,traint), exes) -> vs
  | `BBDCL_val (vs,t) -> vs
  | `BBDCL_var (vs,t) -> vs
  | `BBDCL_ref (vs,t) -> vs
  | `BBDCL_tmp (vs,t) -> vs
  | `BBDCL_glr (props,vs,ret, (p,exes)) -> vs
  | `BBDCL_regmatch (props,vs,(ps,traint),ret,(alpha,states,h,mx))  -> vs
  | `BBDCL_reglex (props,vs,(ps,traint),le,ret,(alpha,states,h,mx)) -> vs
  | `BBDCL_class (props,vs) -> vs
  | `BBDCL_union (vs,ps) -> vs
  | `BBDCL_struct (vs,ps) -> vs
  | `BBDCL_cstruct (vs,ps) -> vs
  | `BBDCL_newtype (vs,t) -> vs
  | `BBDCL_cclass (vs,ps) -> vs
  | `BBDCL_const (_,vs,t,ct,reqs) -> vs
  | `BBDCL_insert (vs,s,ikind,reqs) -> vs
  | `BBDCL_fun (props,vs,argtypes,ret,ct,reqs,prec) -> vs
  | `BBDCL_callback (props,vs,argtypes_cf,argtypes_c,k,ret,reqs,prec) -> vs
  | `BBDCL_proc (props,vs,argtypes,ct,reqs) -> vs
  | `BBDCL_abs (vs,tqual,ct,reqs) ->  vs
  | `BBDCL_nonconst_ctor (vs,uidx,udt, ctor_idx, ctor_argt, evs, etraint) -> vs
  | `BBDCL_typeclass (props,vs) ->  vs
  | `BBDCL_instance (props,vs,con,tc,ts) -> vs
