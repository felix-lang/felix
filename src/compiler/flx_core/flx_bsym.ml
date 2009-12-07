open Flx_types

(** The bound symbol type. *)
type t = string * Flx_types.bid_t option * Flx_srcref.t * Flx_types.bbdcl_t

(** Return if the bound symbol is an identity function. *)
let is_identity (_,_,_,bbdcl) =
  match bbdcl with
  | BBDCL_fun (_,_,_,_,Flx_ast.CS_identity,_,_) -> true
  | _ -> false

(** Return if the bound symbol is a variable. *)
let is_variable (_,_,_,bbdcl) =
  match bbdcl with
  | BBDCL_var _ | BBDCL_val _ -> true
  | _ -> false

let is_global_var (_,parent,_,bbdcl) =
  match bbdcl with
  | BBDCL_var _
  | BBDCL_val _ when (match parent with None -> true | _ -> false ) -> true
  | _ -> false

(** Return if the bound symbol is a function or procedure. *)
let is_function (_,_,_,bbdcl) =
  match bbdcl with
  | BBDCL_function _ | BBDCL_procedure _ -> true
  | _ -> false

(** Return if the bound symbol is a generator. *)
let is_generator (_,_,_,bbdcl) =
  match bbdcl with
  | BBDCL_fun (props,_,_,_,_,_,_)
  | BBDCL_function (props,_,_,_,_) when List.mem `Generator props -> true
  | _ -> false

(** Returns the bound type value list of the bound symbol. *)
let get_bvs (_,_,_,bbdcl) =
  match bbdcl with
  | BBDCL_function (props,bvs,(ps,traint),ret,exes) -> bvs
  | BBDCL_procedure (props,bvs,(ps,traint),exes) -> bvs
  | BBDCL_val (bvs,t) -> bvs
  | BBDCL_var (bvs,t) -> bvs
  | BBDCL_ref (bvs,t) -> bvs
  | BBDCL_tmp (bvs,t) -> bvs
  | BBDCL_union (bvs,ps) -> bvs
  | BBDCL_struct (bvs,ps) -> bvs
  | BBDCL_cstruct (bvs,ps) -> bvs
  | BBDCL_newtype (bvs,t) -> bvs
  | BBDCL_const (_,bvs,t,ct,reqs) -> bvs
  | BBDCL_insert (bvs,s,ikind,reqs) -> bvs
  | BBDCL_fun (props,bvs,argtypes,ret,ct,reqs,prec) -> bvs
  | BBDCL_callback (props,bvs,argtypes_cf,argtypes_c,k,ret,reqs,prec) -> bvs
  | BBDCL_proc (props,bvs,argtypes,ct,reqs) -> bvs
  | BBDCL_abs (bvs,tqual,ct,reqs) -> bvs
  | BBDCL_nonconst_ctor (bvs,uidx,udt, ctor_idx, ctor_argt, ebvs, etraint) -> bvs
  | BBDCL_typeclass (props,bvs) -> bvs
  | BBDCL_instance (props,bvs,con,tc,ts) -> bvs
