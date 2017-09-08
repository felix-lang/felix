
open Flx_util
open Flx_list
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bbdcl
open Flx_mtypes2
open Flx_name
open Flx_unify
open Flx_typing
open List
open Flx_print
open Flx_exceptions
open Flx_maps
open Flx_name
open Flx_bid
open Flx_btype_subst

let find_thread_vars_with_type bsym_table =
  let vars = ref [] in
  Flx_bsym_table.iter begin fun k parent bsym ->
    match parent, Flx_bsym.bbdcl bsym with
    | None,BBDCL_val (_,t,(`Val | `Var | `Once)) -> vars := (k,t) :: !vars
    | None,BBDCL_val (_,t,`Ref) -> vars := (k, btyp_pointer t) :: !vars
    | _ -> ()
  end bsym_table;
  !vars


let find_references syms bsym_table index ts =
  let children =
    try
      Flx_bsym_table.find_children bsym_table index
    with Not_found -> BidSet.empty
  in
  let references = ref [] in

  BidSet.iter begin fun idx ->
    try
      let bsym = Flx_bsym_table.find bsym_table idx in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_val (vs,t,(`Val | `Var | `Ref | `Once )) ->
          if length ts <> length vs then begin
            failwith
            (
              "[find_references] entry " ^ string_of_bid index ^
              ", child " ^ Flx_bsym.id bsym ^ "<" ^ string_of_bid idx ^ ">" ^
              ", wrong number of args, expected vs = " ^
              si (length vs) ^
              ", got ts=" ^
              si (length ts)
            )
          end;
          let t = tsubst (Flx_bsym.sr bsym) vs ts t in
          references := (idx,t) :: !references
      | _ -> ()
    with Not_found -> ()
  end children;

  !references



