open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_exceptions
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_typing2
open Flx_unify
open Flx_beta
open Flx_generic
open Flx_overload
open Flx_tpat
open Flx_lookup_state
open Flx_name_map
open Flx_btype_occurs
open Flx_btype_subst
open Flx_bid

(** Find the type of a bound symbol. *)
let btype_of_bsym state bsym_table sr bt bid bsym =
  match Flx_bsym.bbdcl bsym with
  | BBDCL_label _ -> btyp_label ()
  | BBDCL_invalid -> assert false
  | BBDCL_nominal_type_alias _ -> assert false
  | BBDCL_structural_type_alias _ -> assert false
  | BBDCL_module -> 
    clierrx "[flx_bind/flx_lookup.ml:1854: E106] " (Flx_bsym.sr bsym) ("Attempt to find type of module or library name " ^ Flx_bsym.id bsym)

  | BBDCL_fun (_,_,params,return_type,effects,_) ->
    begin match effects with
    | BTYP_tuple [] ->
      btyp_function (Flx_bparams.get_btype params, return_type)
    | _ ->
      btyp_effector (Flx_bparams.get_btype params, effects, return_type)
    end
  | BBDCL_val (_,t,_) -> t
  | BBDCL_newtype (_,t) -> t
  | BBDCL_external_type _ -> clierr2 sr (Flx_bsym.sr bsym) ("Use type as if variable: " ^ Flx_bsym.id bsym)
  | BBDCL_external_const (_,_,t,_,_) -> t
  | BBDCL_external_fun (_,_,params,return_type,_,_,_) ->
      btyp_function (btyp_tuple params, return_type)
  | BBDCL_external_code _ -> assert false
  | BBDCL_union (_,ls) ->
      btyp_variant (List.map (fun (n,_,evs,d,c,gadt) -> n,d) ls)
  | BBDCL_struct (_,ls)
  | BBDCL_cstruct (_,ls,_) ->
(*
print_endline "Type of struct, considered as constructor function";
*)
     let _,vs,_ = find_split_vs state.sym_table bsym_table bid in
(*
print_endline ("btype of bsym struct " ^ Flx_bsym.id bsym ^ "<" ^ si bid ^ ">, #vs =" ^ si (List.length vs));
*)
      (* Lower a struct type into a function that creates the struct. *)
      let ts = List.map
        (fun (s,i,_) -> TYP_name (Flx_bsym.sr bsym,s,[]))
        vs 
      in
      let ts = List.map (bt (Flx_bsym.sr bsym)) ts in
      let ts = adjust_ts
        state.sym_table
        bsym_table
        (Flx_bsym.sr bsym)
        bid
        ts
      in
      let t = btyp_tuple (List.map snd ls) in
      let result = btyp_function (t, btyp_inst (bid, ts, Flx_kind.KIND_type)) in
(*
print_endline ("struct as function [btype_of_bsym] " ^ sbt bsym_table result);
*)
      result

  | BBDCL_instance_type _
  | BBDCL_virtual_type _
  | BBDCL_typeclass _ 
  | BBDCL_instance _ 
  | BBDCL_const_ctor _ 
  | BBDCL_nonconst_ctor _ 
  | BBDCL_axiom 
  | BBDCL_lemma 
  | BBDCL_reduce ->
    clierrx "[flx_bind/flx_lookup.ml:1898: E107] " (Flx_bsym.sr bsym) 
    ("Use entity as if variable:" ^ Flx_bsym.id bsym)
 

