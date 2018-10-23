open Flx_list
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_typing2
open Flx_pat
open Flx_exceptions


let merge_vs
  (vs1,{raw_type_constraint=con1; raw_typeclass_reqs=rtcr1})
  (vs2,{raw_type_constraint=con2; raw_typeclass_reqs=rtcr2})
:vs_list_t =
  let t = `TYP_typeop (Flx_srcref.dummy_sr, "_staticbool_and", `TYP_type_tuple [con1; con2], KND_bool)
  and rtcr = uniq_list (rtcr1 @ rtcr2)
  in
  vs1 @ vs2,
  { raw_type_constraint=t; raw_typeclass_reqs=rtcr}


