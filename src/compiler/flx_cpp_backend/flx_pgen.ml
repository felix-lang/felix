open Flx_list
open Flx_ast
open Flx_types
open Flx_mtypes2
open Flx_typing
open Flx_name
open Flx_unify
open Flx_csubst
open List
open Flx_ctypes
open Flx_cexpr
open Flx_maps

let shape_of syms bsym_table tn t =
  match t with
  | BTYP_inst (i,ts) ->
    begin match Flx_bsym_table.find_bbdcl bsym_table i with
    | BBDCL_union (vs,idts) ->
      let varmap = mk_varmap vs ts in
      let cpts = map (fun (_,_,t) -> varmap_subst varmap t) idts in
      if all_voids cpts then "_int_ptr_map"
      else "_uctor_ptr_map"
    | _ -> tn t ^ "_ptr_map"
    end
  | BTYP_sum cpts ->
      if all_units cpts then "_int_ptr_map"
      else "_uctor_ptr_map"

  | BTYP_pointer _ -> "_address_ptr_map"
  | _ -> tn t ^ "_ptr_map"

let gen_prim_call
  syms
  bsym_table
  (tsub:btypecode_t -> btypecode_t)
  (ge: Flx_srcref.t -> tbexpr_t -> cexpr_t)
  (ct:string)
  (ts:btypecode_t list)
  ((arg,argt as a) : tbexpr_t)
  ret sr sr2 prec
=
  (*
  print_endline ("ct= "^ct);
  print_endline ("ts= "^catmap "," (sbt sym_table) ts);
  print_endline ("argt = " ^ sbt sym_table argt);
  print_endline ("arg = " ^ sbe sym_table a);
  *)
  let tn t = cpp_typename syms bsym_table t in
  let rt t = reduce_type (tsub t) in
  let rtn t = tn (rt t) in

  let argt = rt argt in
  let tt = tn argt in
  let sh t = shape_of syms bsym_table tn t in
  let gshapes = map sh ts in
  let ts = map rtn ts in
  let carg =
    match argt with
    | BTYP_tuple []  -> ce_atom "UNIT_VALUE_ERROR"
    | x -> ge sr a
  in
  let ashape = sh argt in
  match arg,argt with

  (* the argument is explicitly a tuple *)
  | (BEXPR_tuple es,_) ->
    let ess =
      map
      (fun e->
        match e with
        (* individual arguments which are unit values are never passed:
          they CAN be passed as subcomponents though .. but they can't
          be generated .. we need to fix this!
        *)
        | BEXPR_tuple [],_ ->
          (*
          print_endline "Stripping unit";
          *)
          `Ce_atom "/*()*/"

        | _ -> ge sr e
      )
      es
    in
    let ets,ashapes =
      match argt with
      | BTYP_tuple typs -> map rtn typs, map sh typs
      | BTYP_array (t,BTYP_unitsum n) ->
        let t = tn t
        and s = sh t
        in rev_map (fun _ -> t) (nlist n), rev_map (fun _ -> s) (nlist n)
      | _ -> assert false
    in
    csubst sr sr2 ct carg ess ets tt ret ts prec ashape ashapes ["Error"] gshapes

  (* the argument isnt a tuple, but the type is *)
  | (_,BTYP_tuple typs) as x ->
    let n = length typs in
    let typs = map rt typs in
    let es =
      map2
      (fun i t -> BEXPR_get_n (i,x),t)
      (nlist n) typs
    in
    let ess = map (ge sr) es in
    let ets = map tn typs in
    csubst sr sr2 ct carg ess ets tt ret ts prec ashape (map sh typs) ["Error"] gshapes

  (* the argument isnt a tuple, but the type is an array *)
  | (_,(BTYP_array(t,BTYP_unitsum n) as ta)) as x ->
    let t = rt t in
    let typs = map (fun _ -> rt t) (nlist n) in
    let es =
      map
      (fun i -> BEXPR_get_n (i,x),t)
      (nlist n)
    in
    let ess = map (ge sr) es in
    let ets = map tn typs in
    csubst sr sr2 ct carg ess ets tt ret ts prec ashape (map sh typs) ["error"] gshapes

  (* the argument isn't an explicit tuple, and the type
     is neither an array nor tuple
  *)
  | (_,typ) ->
    csubst sr sr2 ct carg [carg] [tt] tt ret ts prec ashape [ashape] ["Error"] gshapes
