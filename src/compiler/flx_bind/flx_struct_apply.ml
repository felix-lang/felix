open Flx_btype
open Flx_bexpr
open Flx_btype_subst

let cal_struct_apply 
  bsym_table state bind_type' mkenv build_env cal_apply
  rs sr f (ea, ta as a) i ts' 
=
  let id,vs,fls = match Flx_lookup_state.hfind "lookup" state.Flx_lookup_state.sym_table i with
    | { Flx_sym.id=id; vs=vs; symdef=Flx_types.SYMDEF_struct ls }
    | { Flx_sym.id=id; vs=vs; symdef=Flx_types.SYMDEF_cstruct (ls,_) } -> id,vs,ls
    | _ -> assert false
  in
  let _,vs,_  = Flx_generic.find_split_vs state.Flx_lookup_state.sym_table bsym_table i in
  let alst = match ta with
    |BTYP_record (ts) -> ts
    | _ -> assert false
  in
  let nf = List.length fls in
  let na = List.length alst in
  if nf <> na then Flx_exceptions.clierrx "[flx_bind/flx_struct_apply.ml:20: E252] " sr
    (
      "Wrong number of components matching record argument to struct"
    )
  ;
  let bvs = List.map
    (fun (n,i,mt) -> n, btyp_type_var (i,Flx_btype.bmt "Flx_struct_apply1" mt))
    (vs)
  in
  let env' = build_env state bsym_table (Some i) in
  let vs' = List.map (fun (s,i,tp) -> s,i,Flx_btype.bmt "Flx_struct_apply2" tp) (vs) in
  let ialst = List.map2 (fun (k,t) i -> k,(t,i)) alst (Flx_list.nlist na) in
  let a =
    List.map (fun (name,ct)->
      let (t,j) =
        try List.assoc name ialst
        with Not_found -> Flx_exceptions.clierrx "[flx_bind/flx_struct_apply.ml:36: E253] " sr ("struct component " ^ name ^ " not provided by record")
      in
    let ct = bind_type' state bsym_table env' Flx_lookup_state.rsground sr ct bvs mkenv in
    let ct = tsubst sr vs' ts' ct in
      if Flx_unify.type_eq bsym_table state.Flx_lookup_state.counter ct t then begin
        bexpr_get_n t j a
      end else Flx_exceptions.clierrx "[flx_bind/flx_struct_apply.ml:42: E254] " sr ("Component " ^ name ^
        " struct component type " ^ Flx_print.sbt bsym_table ct ^
        "\ndoesn't match record type " ^ Flx_print.sbt bsym_table t
      )
    )
    fls
  in
  let cts = List.map snd a in
  let t = match cts with [t] -> t | _ -> btyp_tuple cts in
  let a = match a with [x,_] -> x,t | _ -> bexpr_tuple t a in
  cal_apply state bsym_table sr rs f a


