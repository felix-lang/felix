open Flx_types

(** The bound symbol type. *)
type t = {
  id: string;
  sr: Flx_srcref.t;
  parent: bid_t option;
  vs:Flx_types.ivs_list_t;
  pubmap:Flx_types.name_map_t;
  privmap:Flx_types.name_map_t;
  dirs:Flx_types.sdir_t list;
  bbdcl: bbdcl_t;
}

(** Return if the bound symbol is an identity function. *)
let is_identity bsym =
  match bsym.bbdcl with
  | BBDCL_fun (_,_,_,_,Flx_ast.CS_identity,_,_) -> true
  | _ -> false

(** Return if the bound symbol is a variable. *)
let is_variable bsym =
  match bsym.bbdcl with
  | BBDCL_var _ | BBDCL_val _ -> true
  | _ -> false

let is_global_var bsym =
  match bsym.bbdcl with
  | BBDCL_var _
  | BBDCL_val _ when (match bsym.parent with None -> true | _ -> false ) -> true
  | _ -> false

(** Return if the bound symbol is a function or procedure. *)
let is_function bsym =
  match bsym.bbdcl with
  | BBDCL_function _ | BBDCL_procedure _ -> true
  | _ -> false

(** Return if the bound symbol is a generator. *)
let is_generator bsym =
  match bsym.bbdcl with
  | BBDCL_fun (props,_,_,_,_,_,_)
  | BBDCL_function (props,_,_,_,_) when List.mem `Generator props -> true
  | _ -> false

(** Returns the bound type value list of the bound symbol. *)
let get_bvs bsym =
  Flx_types.bvs_of_bbdcl bsym.bbdcl
