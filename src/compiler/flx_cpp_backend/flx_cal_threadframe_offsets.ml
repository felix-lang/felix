
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
open Flx_cal_type_offsets
open Flx_gen_shape
open Flx_findvars


let gen_thread_frame_offsets module_name s h syms bsym_table new_table =
  let vars = find_thread_vars_with_type bsym_table in
  let ts = [] in
  let name = "thread_frame_t" in
  let offsets =
    concat
    (
      map
      (fun (idx,typ)->
        let mem = cpp_instance_name syms bsym_table idx ts in
        let offsets = get_offsets' syms bsym_table typ in
        map
        (fun offset ->
          add_offset (offsetof name mem) offset
        )
        offsets
      )
      vars
    )
  in
  let n = length offsets in
  bcat s ("\n//**************************************\n");
  bcat s ( "//SHAPE for "^ name ^ "\n");
  gen_offset_data syms bsym_table module_name s h n name offsets 
    false false [] (Some "::flx::gc::generic::gc_flags_immobile") 
    "0" "0" new_table



