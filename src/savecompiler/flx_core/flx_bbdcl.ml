open Flx_ast
open Flx_types
open Flx_bid
open Flx_kind (* for bvs_t *)

module CS = Flx_code_spec

type btype_qual_t = [
  | Flx_ast.base_type_qual_t
  | `Bound_needs_shape of Flx_btype.t
  | `Scanner of CS.t
  | `Finaliser of CS.t
  | `Encoder of CS.t
  | `Decoder of CS.t
]


(** Used to represent all the different value types. *)
(** Once means linear type, it has to be a val *)
type value_kind_t = [`Val | `Var | `Ref | `Tmp | `Once]

(** Used to represent all the different external function types. *)
type external_fun_kind_t = [
  | `Code of CS.t
  | `Callback of Flx_btype.t list * int
]

(** Bound declarations. *)
type t =
  | BBDCL_virtual_type of bvs_t
  | BBDCL_invalid
  | BBDCL_module
  | BBDCL_label of      string
  | BBDCL_fun of        property_t list * bvs_t * Flx_bparams.t * Flx_btype.t *
                        Flx_btype.t * (* effects *)
                        Flx_bexe.t list
  | BBDCL_val of        bvs_t * Flx_btype.t * value_kind_t

  (* binding structures [prolog] *)
  | BBDCL_newtype of    bvs_t * Flx_btype.t
  | BBDCL_nominal_type_alias of bvs_t * Flx_btype.t
  | BBDCL_structural_type_alias of bvs_t * Flx_btype.t
  | BBDCL_instance_type of    bvs_t * Flx_btype.t
  | BBDCL_external_type of
                        bvs_t * btype_qual_t list * CS.t * Flx_btype.breqs_t
  | BBDCL_external_const of
                        property_t list * bvs_t * Flx_btype.t * CS.t *
                        Flx_btype.breqs_t
  | BBDCL_external_fun of
                        property_t list * bvs_t * Flx_btype.t list *
                        Flx_btype.t * Flx_btype.breqs_t * prec_t * external_fun_kind_t
  | BBDCL_external_code of
                        bvs_t * CS.t * ikind_t * Flx_btype.breqs_t

  | BBDCL_union of      bvs_t * (Flx_id.t * int * bvs_t * Flx_btype.t * Flx_btype.t * bool) list 
  | BBDCL_struct of     bvs_t * (Flx_id.t * Flx_btype.t) list
  | BBDCL_cstruct of    bvs_t * (Flx_id.t * Flx_btype.t) list * Flx_btype.breqs_t
  | BBDCL_typeclass of  property_t list * bvs_t
  | BBDCL_instance of   property_t list *
                        bvs_t *
                        Flx_btype.t (* constraint *) *
                        bid_t *
                        Flx_btype.t list
  | BBDCL_const_ctor of bvs_t * bid_t * Flx_btype.t * int * 
                        bvs_t * Flx_btype.t (* existentials and constraint for GADTs *)
  | BBDCL_nonconst_ctor of bvs_t * bid_t * Flx_btype.t * int * Flx_btype.t *
                        bvs_t * Flx_btype.t (* existentials and constraint for GADTs *)
  | BBDCL_axiom
  | BBDCL_lemma
  | BBDCL_reduce

(* -------------------------------------------------------------------------- *)

let bbdcl_label s = BBDCL_label s

let bbdcl_invalid () =
  BBDCL_invalid

let bbdcl_module () =
  BBDCL_module

let bbdcl_fun (prop, bvs, ps, res, effects, es) =
  BBDCL_fun (prop, bvs, ps, res, effects, es)


let bbdcl_val (bvs, t, kind) =
(*
  begin match kind with
  | `Val when Flx_btype.contains_uniq t ->
     print_endline ("WARNING, val's should not be or contain uniq values, use a var : type = " ^ 
       Flx_btype.st t)
  | _ ->  ()
  end;
*)
  BBDCL_val (bvs, t, kind)

let bbdcl_newtype (bvs, t) =
  BBDCL_newtype (bvs, t)

let bbdcl_nominal_type_alias (bvs, t) =
  BBDCL_nominal_type_alias (bvs, t)

let bbdcl_structural_type_alias (bvs, t) =
  BBDCL_structural_type_alias (bvs, t)

let bbdcl_instance_type (bvs, t) =
  BBDCL_instance_type (bvs, t)


let bbdcl_external_type (bvs, quals, code, breqs) =
  BBDCL_external_type (bvs, quals, code, breqs)

let bbdcl_external_const (prop, bvs, t, code, breqs) =
  BBDCL_external_const (prop, bvs, t, code, breqs)

let bbdcl_external_fun (prop, bvs, ps, rt, breqs, prec, kind) =
  BBDCL_external_fun (prop, bvs, ps, rt, breqs, prec, kind)

let bbdcl_external_code (bvs, code, ikind, breqs) =
  BBDCL_external_code (bvs, code, ikind, breqs)

let bbdcl_union (bvs, cs) =
  BBDCL_union (bvs, cs)

let bbdcl_struct (bvs, cs) =
  BBDCL_struct (bvs, cs)

let bbdcl_cstruct (bvs, cs, breqs) =
  BBDCL_cstruct (bvs, cs, breqs)

let bbdcl_typeclass (prop, bvs) =
  BBDCL_typeclass (prop, bvs)

let bbdcl_instance (prop, bvs, cons, bid, ts) =
  BBDCL_instance (prop, bvs, cons, bid, ts)

let bbdcl_const_ctor (bvs, uidx, ut, ctor_idx, evs, etraint) =
  BBDCL_const_ctor (bvs, uidx, ut, ctor_idx, evs, etraint)

let bbdcl_nonconst_ctor (bvs, uidx, ut, ctor_idx, ctor_argt, evs, etraint) =
  BBDCL_nonconst_ctor (bvs, uidx, ut, ctor_idx, ctor_argt, evs, etraint)

let bbdcl_axiom () =
  BBDCL_axiom

let bbdcl_reduce () =
  BBDCL_reduce

let bbdcl_lemma () =
  BBDCL_lemma

let bbdcl_virtual_type bvs = 
  BBDCL_virtual_type bvs

(* -------------------------------------------------------------------------- *)

(** Extract the parameters of a bound declaration. *)
let get_bparams = function
  | BBDCL_invalid -> assert false
  | BBDCL_fun (_,_,ps,_,_,_) -> ps
  | _ -> assert false

(** Extract the types of a bound declaration. *)
let get_ts = function
  | BBDCL_invalid -> assert false
  | BBDCL_instance (_, _, _, _, ts) -> ts
  | _ -> []

(** Extract the bound type variables of a bound declaration. *)
let get_bvs = function
  | BBDCL_virtual_type bvs -> bvs
  | BBDCL_invalid -> assert false
  | BBDCL_module -> []
  | BBDCL_fun (_, bvs, _, _, _,_) -> bvs
  | BBDCL_val (bvs, _, _) -> bvs
  | BBDCL_newtype (bvs, _) -> bvs
  | BBDCL_nominal_type_alias (bvs, _) -> bvs
  | BBDCL_structural_type_alias (bvs, _) -> bvs
  | BBDCL_instance_type (bvs, _) -> bvs
  | BBDCL_external_type (bvs, _, _, _) -> bvs
  | BBDCL_external_const (_, bvs, _, _, _) -> bvs
  | BBDCL_external_fun (_, bvs, _, _, _, _, _) -> bvs
  | BBDCL_external_code (bvs, _, _, _) -> bvs
  | BBDCL_union (bvs, _) -> bvs
  | BBDCL_struct (bvs, _) -> bvs
  | BBDCL_cstruct (bvs, _,_) -> bvs
  | BBDCL_typeclass (_, bvs) -> bvs
  | BBDCL_instance (_, bvs, _, _, _) -> bvs
  | BBDCL_const_ctor (bvs, _, _, _, _, _) -> bvs
  | BBDCL_nonconst_ctor (bvs, _, _, _, _, _, _) -> bvs
  | BBDCL_axiom -> []
  | BBDCL_lemma -> []
  | BBDCL_reduce -> []
  | BBDCL_label _ -> []

(* -------------------------------------------------------------------------- *)

(** Return whether or not the bound declaration is valid. *)
let is_valid = function
  | BBDCL_invalid -> false
  | _ -> true

(* -------------------------------------------------------------------------- *)

let iter
  ?(f_bid=fun _ -> ())
  ?(f_btype=fun _ -> ())
  ?(f_bexpr=fun _ -> ())
  ?(f_bexe=fun _ -> ())
  bbdcl
=
  (* Note: we're ignoring bvs on purpose here, since while bvs_t has bid_t in
   * it's type, it's just used to uniquely named the type variables, and has no
   * corresponding value number in the symbol table. *)

  let f_ps ps = Flx_bparams.iter ~f_bid ~f_btype ps in
  let f_breqs =
    List.iter begin fun (bid,ts) ->
      f_bid bid;
      List.iter f_btype ts
    end
  in
  let f_btype_qual = function
    | #base_type_qual_t -> ()
    | `Bound_needs_shape t -> f_btype t
    | `Scanner cs -> ()
    | `Finaliser cs -> ()
    | `Encoder cs -> ()
    | `Decoder cs -> ()
  in
  match bbdcl with
  | BBDCL_label _ -> ()
  | BBDCL_invalid -> ()
  | BBDCL_module -> ()
  | BBDCL_fun (_,_,ps,res,effects,es) ->
      f_ps ps;
      f_btype effects;
      f_btype res;
      List.iter f_bexe es
  | BBDCL_val (_,t,`Ref) -> f_btype (Flx_btype.btyp_pointer t)
  | BBDCL_val (_,t,_) -> f_btype t
  | BBDCL_virtual_type bvs -> ()

  | BBDCL_newtype (_,t) -> f_btype t
  | BBDCL_nominal_type_alias (_,t) -> f_btype t
  | BBDCL_structural_type_alias (_,t) -> f_btype t
  | BBDCL_instance_type (_,t) -> f_btype t
  | BBDCL_external_type (_,quals,_,breqs) ->
      List.iter f_btype_qual quals;
      f_breqs breqs
  | BBDCL_external_const (_,_,t,_,breqs) ->
      f_btype t;
      f_breqs breqs
  | BBDCL_external_fun (_,_,ps,rt,breqs,_,kind) ->
      List.iter f_btype ps;
      f_btype rt;
      begin match kind with
      | `Callback (ps_c,_) -> List.iter f_btype ps_c
      | _ -> ()
      end;
      f_breqs breqs
  | BBDCL_external_code (_,_,_,breqs) ->
      f_breqs breqs
  | BBDCL_union (_,cs) ->
      List.iter (fun (_,_,evs,d,c,_) -> f_btype d; f_btype c) cs
  | BBDCL_struct (_,cs) ->
      List.iter (fun (n,t) -> f_btype t) cs
  | BBDCL_cstruct (_,cs,breqs) ->
      List.iter (fun (n,t) -> f_btype t) cs;
      f_breqs breqs
  | BBDCL_typeclass (_,_) -> ()
  | BBDCL_instance (_,_,cons,bid,ts) ->
      f_btype cons;
      f_bid bid;
      List.iter f_btype ts
  | BBDCL_const_ctor (_,uidx,ut,_,_,etraint) ->
      f_bid uidx;
      f_btype ut;
      f_btype etraint
  | BBDCL_nonconst_ctor (_,uidx,ut,_,ctor_argt,_,etraint) ->
      f_bid uidx;
      f_btype ut;
      f_btype ctor_argt;
      f_btype etraint
  | BBDCL_axiom -> ()
  | BBDCL_lemma -> ()
  | BBDCL_reduce -> ()

let map
  ?(f_bid=fun i -> i)
  ?(f_btype=fun t -> t)
  ?(f_bexpr=fun e -> e)
  ?(f_bexe=fun e -> e)
  bbdcl
=
  let f_ps ps = Flx_bparams.map ~f_bid ~f_btype ps in
  let f_breqs =
    List.map begin fun (bid,ts) ->
      f_bid bid, List.map f_btype ts
    end
  in
  let f_btype_qual = function
    | #base_type_qual_t as qual -> qual
    | `Bound_needs_shape t -> `Bound_needs_shape (f_btype t)
    | `Scanner cs -> `Scanner cs
    | `Finaliser cs -> `Finaliser cs
    | `Encoder cs -> `Encoder cs
    | `Decoder cs -> `Decoder cs
  in
  match bbdcl with
  | BBDCL_label s -> bbdcl

  | BBDCL_invalid -> bbdcl
  | BBDCL_module -> bbdcl
  | BBDCL_fun (props,bvs,ps,res,effects,es) ->
      BBDCL_fun (props,bvs,f_ps ps,f_btype res,f_btype effects, List.map f_bexe es)
  | BBDCL_val (bvs,t,`Ref) ->
      bbdcl_val (bvs,f_btype (Flx_btype.btyp_pointer t),`Ref)
  | BBDCL_val (bvs,t,kind) -> bbdcl_val (bvs,f_btype t,kind)
  | BBDCL_newtype (bvs,t) -> BBDCL_newtype (bvs,f_btype t)
  | BBDCL_nominal_type_alias (bvs,t) -> BBDCL_nominal_type_alias (bvs,f_btype t)
  | BBDCL_structural_type_alias (bvs,t) -> BBDCL_structural_type_alias (bvs,f_btype t)
  | BBDCL_instance_type (bvs,t) -> BBDCL_instance_type (bvs,f_btype t)
  | BBDCL_virtual_type bvs -> BBDCL_virtual_type bvs

  | BBDCL_external_type (bvs,quals,code,breqs) ->
      BBDCL_external_type (bvs,List.map f_btype_qual quals,code,f_breqs breqs)
  | BBDCL_external_const (props,bvs,t,code,breqs) ->
      BBDCL_external_const (props,bvs,f_btype t,code,f_breqs breqs)
  | BBDCL_external_fun (props,bvs,ps,rt,breqs,prec,kind) ->
      BBDCL_external_fun (
        props,
        bvs,
        List.map f_btype ps,
        f_btype rt,
        f_breqs breqs,
        prec,
        begin match kind with
        | `Callback (ps_c,k) -> `Callback (List.map f_btype ps_c,k)
        | _ -> kind
        end)
  | BBDCL_external_code (bvs,code,ikind,breqs) ->
      BBDCL_external_code (bvs,code,ikind,f_breqs breqs)
  | BBDCL_union (bvs,cs) ->
      BBDCL_union (bvs,List.map (fun (n,i,evs,d,c,gadt) -> n,i,evs,f_btype d,f_btype c,gadt) cs)
  | BBDCL_struct (bvs,cs) ->
      BBDCL_struct (bvs,List.map (fun (n,t) -> n,f_btype t) cs)
  | BBDCL_cstruct (bvs,cs, breqs) ->
      BBDCL_cstruct (bvs,List.map (fun (n,t) -> n,f_btype t) cs, f_breqs breqs)
  | BBDCL_typeclass (props,bvs) -> bbdcl
  | BBDCL_instance (props,bvs,cons,bid,ts) ->
      BBDCL_instance (
        props,
        bvs,
        f_btype cons,
        f_bid bid,
        List.map f_btype ts)
  | BBDCL_const_ctor (bvs,uidx,ut,ctor_idx,evs,etraint) ->
      BBDCL_const_ctor (
        bvs,
        f_bid uidx,
        f_btype ut,
        f_bid ctor_idx,
        evs,
        f_btype etraint)
  | BBDCL_nonconst_ctor (bvs,uidx,ut,ctor_idx,ctor_argt,evs,etraint) ->
      BBDCL_nonconst_ctor (
        bvs,
        f_bid uidx,
        f_btype ut,
        f_bid ctor_idx,
        f_btype ctor_argt,
        evs,
        f_btype etraint)
  | BBDCL_axiom -> bbdcl
  | BBDCL_lemma -> bbdcl
  | BBDCL_reduce -> bbdcl

(* -------------------------------------------------------------------------- *)

(** Calls the function over every bid inside the bound symbol. *)
let iter_uses f bbdcl =
  let f_btype = Flx_btype.flat_iter ~f_bid:f in
  let f_bexpr = Flx_bexpr.iter ~f_bid:f ~f_btype in
  let f_bexe = Flx_bexe.iter ~f_bid:f ~f_btype ~f_bexpr in
  let f_ps ps = Flx_bparams.iter ~f_btype ~f_bexpr ps in
  let f_breqs =
    List.iter begin fun (bid,ts) ->
      List.iter f_btype ts
    end
  in
  let f_btype_qual = function
    | #base_type_qual_t -> ()
    | `Bound_needs_shape t -> f_btype t
    | `Scanner cs -> ()
    | `Finaliser cs -> ()
    | `Encoder cs -> ()
    | `Decoder cs -> ()
  in
  match bbdcl with
  | BBDCL_fun (_,_,ps,res,effects,_) ->
      f_ps ps;
      f_btype res;
      f_btype effects

  | BBDCL_external_type (_,quals,_,breqs) ->
      List.iter f_btype_qual quals;
      f_breqs breqs
  | BBDCL_external_const (_,_,t,_,breqs) ->
      f_btype t;
      f_breqs breqs
  | BBDCL_external_fun (_,_,ps,rt,breqs,_,kind) ->
      List.iter f_btype ps;
      f_btype rt;
      f_breqs breqs;
      begin match kind with
      | `Callback (ps_c,_) -> List.iter f_btype ps_c
      | _ -> ()
      end
  | BBDCL_external_code (_,_,_,breqs) ->
      f_breqs breqs
  | BBDCL_cstruct (_,_,breqs) ->
      f_breqs breqs

  | _ ->
      iter ~f_bid:f ~f_btype ~f_bexpr ~f_bexe bbdcl



