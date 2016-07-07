(** The bound symbol type. *)
type t = {
  id:Flx_id.t;
  sr:Flx_srcref.t;
  bbdcl:Flx_bbdcl.t;
}

let create
  ?(sr=Flx_srcref.dummy_sr)
  id bbdcl
=
  { id=id;
    sr=sr;
    bbdcl=bbdcl }

let of_sym sym bbdcl =
  { id=sym.Flx_sym.id;
    sr=sym.Flx_sym.sr;
    bbdcl=bbdcl }

let replace_bbdcl bsym bbdcl = { bsym with bbdcl=bbdcl }

let id bsym = bsym.id
let sr bsym = bsym.sr
let bbdcl bsym = bsym.bbdcl

(** Return if the bound symbol is an identity function. *)
let is_identity bsym =
  match bsym.bbdcl with
  | Flx_bbdcl.BBDCL_external_fun (_,_,_,_,_,_,kind) ->
      begin match kind with
      | `Code Flx_code_spec.Identity -> true
      | _ -> false
      end
  | _ -> false

(** Return if the bound symbol is a variable. *)
let is_variable bsym =
  match bsym.bbdcl with
  | Flx_bbdcl.BBDCL_val _ -> true
  | _ -> false

(** Return if the bound symbol is a function or procedure. *)
let is_function bsym =
  match bsym.bbdcl with
  | Flx_bbdcl.BBDCL_fun _ -> true
  | _ -> false

(** Return if the bound symbol is a generator. *)
let is_generator bsym =
  match bsym.bbdcl with
  | Flx_bbdcl.BBDCL_fun (props,_,_,_,_,_)
  | Flx_bbdcl.BBDCL_external_fun (props,_,_,_,_,_,_)
    when List.mem `Generator props -> true
  | _ -> false

(** Returns the bound parameters of the bound symbol. *)
let get_bparams bsym =
  Flx_bbdcl.get_bparams bsym.bbdcl

(** Returns the bound type value list of the bound symbol. *)
let get_bvs bsym =
  Flx_bbdcl.get_bvs bsym.bbdcl

(** Calls the function over every bid inside the bound symbol. *)
let iter_uses f bsym = Flx_bbdcl.iter_uses f bsym.bbdcl



