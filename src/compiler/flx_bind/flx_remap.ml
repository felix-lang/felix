open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
open Flx_bbdcl

(** Remaps a bound index by adding an offset to it. *)
let remap_bid offset bid = bid + offset

(** Remaps bound types by adding an offset to the bound index. *)
let rec remap_btype offset btype =
  Flx_btype.map
    ~f_bid:(remap_bid offset)
    ~f_btype:(remap_btype offset)
    btype




(** Remap bound types by adding an offset to the bound index. *)
let rec remap_tbexpr offset bexpr =
  Flx_bexpr.map
    ~f_bid:(remap_bid offset)
    ~f_btype:(remap_btype offset)
    ~f_bexpr:(remap_tbexpr offset)
    bexpr


(** Remap bound exes by adding an offset to the bound index. *)
let remap_bexe offset bexe =
  Flx_bexe.map
    ~f_bid:(remap_bid offset)
    ~f_btype:(remap_btype offset)
    ~f_bexpr:(remap_tbexpr offset)
    bexe


(** Remaps bound declarations by adding an offset to the bound index. *)
let remap_bbdcl offset bbdcl =
  let remap_bid = remap_bid offset in
  let remap_btype = remap_btype offset in
  let remap_tbexpr = remap_tbexpr offset in
  let remap_bexe = remap_bexe offset in
  let remap_bvs = List.map (fun (n, i, mt) -> n, remap_bid i,mt) in
  let remap_bparams bpar = 
    Flx_bparams.map ~f_bid:remap_bid ~f_btype:remap_btype ~f_bexpr:remap_tbexpr bpar 
  in
  let remap_breqs breqs =
    List.map (fun (i, ts) -> remap_bid i, List.map remap_btype ts) breqs
  in
  match bbdcl with
  | BBDCL_nominal_type_alias _ -> assert false
  | BBDCL_structural_type_alias _ -> assert false
  | BBDCL_label s -> bbdcl_label s
  | BBDCL_invalid ->
      bbdcl_invalid ()

  | BBDCL_module ->
      bbdcl_module ()

  | BBDCL_virtual_type bvs -> bbdcl_virtual_type (remap_bvs bvs)

  | BBDCL_fun (props, vs, ps, res, effects, es) ->
      let vs = remap_bvs vs in
      let ps = remap_bparams ps in
      let res = remap_btype res in
      let effects = remap_btype effects in
      let es = List.map remap_bexe es in
      bbdcl_fun (props, vs, ps, res, effects, es)

  | BBDCL_val (vs, ty, kind) ->
      bbdcl_val (remap_bvs vs, remap_btype ty, kind)

  | BBDCL_newtype (vs, ty) ->
      bbdcl_newtype (remap_bvs vs, remap_btype ty)

  | BBDCL_instance_type (vs, ty) ->
      bbdcl_instance_type (remap_bvs vs, remap_btype ty)


  | BBDCL_external_type (vs, quals, code, reqs) ->
      let vs = remap_bvs vs in
      let quals =
        List.map begin function
          | `Bound_needs_shape t -> `Bound_needs_shape (remap_btype t)
          | qual -> qual
        end quals
      in
      let reqs = remap_breqs reqs in
      bbdcl_external_type (vs, quals, code, reqs)

  | BBDCL_external_const (props, vs, ty, code, reqs) ->
      let vs = remap_bvs vs in
      let ty = remap_btype ty in
      let reqs = remap_breqs reqs in
      bbdcl_external_const (props, vs, ty, code, reqs)

  | BBDCL_external_fun (props, vs, ps, rt, reqs, prec, kind) ->
      let vs = remap_bvs vs in
      let ps = List.map remap_btype ps in
      let rt = remap_btype rt in
      let reqs = remap_breqs reqs in
      let kind =
        match kind with
        | `Code _ -> kind
        | `Callback (ps_c,k) -> `Callback (List.map remap_btype ps_c,k)
      in
      bbdcl_external_fun (props, vs, ps, rt, reqs, prec, kind)

  | BBDCL_external_code (vs, code, ikind, reqs) ->
      let vs = remap_bvs vs in
      let reqs = remap_breqs reqs in
      bbdcl_external_code (vs, code, ikind, reqs)

  | BBDCL_union (vs, cs) ->
      let vs = remap_bvs vs in
      let cs = List.map (fun (n,v,evs,d,c,gadt) -> n,v,remap_bvs evs,remap_btype d, remap_btype c, gadt) cs in
      bbdcl_union (vs, cs)

  | BBDCL_struct (vs, cs) ->
      let vs = remap_bvs vs in
      let cs = List.map (fun (n,t) -> n,remap_btype t) cs in
      bbdcl_struct (vs, cs)

  | BBDCL_cstruct (vs, cs, reqs) ->
      let vs = remap_bvs vs in
      let cs = List.map (fun (n,t) -> n,remap_btype t) cs in
      let reqs = remap_breqs reqs in
      bbdcl_cstruct (vs, cs, reqs)

  | BBDCL_typeclass (props, vs) ->
      bbdcl_typeclass (props, remap_bvs vs)

  | BBDCL_instance (props, vs, cons, bid, ts) ->
      let vs = remap_bvs vs in
      let cons = remap_btype cons in
      let bid = remap_bid bid in
      let ts = List.map remap_btype ts in
      bbdcl_instance (props, vs, cons, bid, ts)

  | BBDCL_const_ctor (vs, uidx, ut, ctor_idx, evs, etraint) ->
      let vs = remap_bvs vs in
      let uidx = remap_bid uidx in
      let ut = remap_btype ut in
      let evs = remap_bvs evs in
      let etraint = remap_btype etraint in
      bbdcl_const_ctor (vs, uidx, ut, ctor_idx, evs, etraint)

  | BBDCL_nonconst_ctor (vs, uidx, ut, ctor_idx, ctor_argt, evs, etraint) ->
      let vs = remap_bvs vs in
      let uidx = remap_bid uidx in
      let ut = remap_btype ut in
      let ctor_argt = remap_btype ctor_argt in
      let evs = remap_bvs evs in
      let etraint = remap_btype etraint in
      bbdcl_nonconst_ctor (vs, uidx, ut, ctor_idx, ctor_argt, evs, etraint)

  | BBDCL_axiom -> bbdcl_axiom ()
  | BBDCL_lemma -> bbdcl_lemma ()
  | BBDCL_reduce -> bbdcl_reduce ()

(** Remap bound interfaces by adding an offset to the bound index. *)
let remap_biface offset biface =
  match biface with
  | BIFACE_export_fun (sr, bid, name) ->
    BIFACE_export_fun (sr, remap_bid offset bid, name)

  | BIFACE_export_cfun (sr, bid, name) ->
    BIFACE_export_cfun (sr, remap_bid offset bid, name)

  | BIFACE_export_python_fun (sr, bid, name) ->
    BIFACE_export_python_fun (sr, remap_bid offset bid, name)

  | BIFACE_export_type (sr, btype, name) ->
    BIFACE_export_type (sr, remap_btype offset btype, name)

  | BIFACE_export_struct (sr, index) ->
    BIFACE_export_struct (sr, remap_bid offset index)

  | BIFACE_export_union(sr, index, name) ->
    BIFACE_export_union (sr, remap_bid offset index, name)

  | BIFACE_export_requirement (sr, breqs) ->
    let remap_bid = remap_bid offset in
    let remap_btype = remap_btype offset in
    let remap_breqs breqs =
      List.map (fun (i, ts) -> remap_bid i, List.map remap_btype ts) breqs
    in
    let breqs = remap_breqs breqs in
    BIFACE_export_requirement (sr, breqs)

(** Remap symbols from an old bound symbol table to a new one by offsetting the
 * bound index by a constant amount. *)
let remap offset in_bsym_table out_bsym_table =
  let rec aux bid parent bsym =
    let bid = remap_bid offset bid in

    (* Skip this bid if we've already processed it. *)
    if Flx_bsym_table.mem out_bsym_table bid then () else

    let bbdcl = remap_bbdcl offset (Flx_bsym.bbdcl bsym) in
    let bsym = Flx_bsym.replace_bbdcl bsym bbdcl in

    match parent with
    | None -> Flx_bsym_table.add out_bsym_table bid None bsym
    | Some parent ->
        aux parent (Some bid) (Flx_bsym_table.find in_bsym_table parent);
        Flx_bsym_table.add out_bsym_table bid (Some parent) bsym
  in

  (* And call this function on every bound symbol. *)
  Flx_bsym_table.iter aux in_bsym_table

