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

let gen_offset_data module_name s n name offsets isfun is_pod props flags last_ptr_map encoder_name decoder_name =
  let this_ptr_map = name ^ "_ptr_map" in
  let old_ptr_map = !last_ptr_map in
  last_ptr_map := "&"^this_ptr_map;
  let noffsets =
    if isfun && mem `Requires_ptf props then si (n-1)^"+FLX_PASS_PTF"
    else si n
  in
  if n <> 0 then
  begin
    bcat s ("static ::std::size_t const " ^ name ^
      "_offsets["^noffsets^ "]={\n");
    bcat s ("  " ^ cat "\n  " offsets);
    bcat s ("\n" ^  "};\n");
    bcat s ("static ::flx::gc::generic::offset_data_t const " ^name^"_offset_data = { " ^ 
     noffsets ^", " ^ name^ "_offsets};\n");
  end;

  if not is_pod then bcat s ("FLX_FINALISER("^name^")\n");
  bcat s (  "static ::flx::gc::generic::gc_shape_t "^ this_ptr_map ^" ={\n");
  bcat s ("  " ^ old_ptr_map ^ ",\n");
  bcat s ("  \"" ^ module_name ^ "::" ^ name ^ "\",\n");
  bcat s ("  1,sizeof("^name^"),\n");
  bcat s ( if not is_pod then ("  "^name^"_finaliser,\n") else ("  0, // finaliser\n"));
  bcat s ("  0, // fcops\n");
  bcat s ("  "^ (if n<>0 then "&"^name^"_offset_data" else "0")^", // scanner data\n");
  bcat s ("  "^ (if n<>0 then "&::flx::gc::generic::scan_by_offsets" else "0")^", // scanner\n");
  bcat s ("  " ^ encoder_name ^",\n");
  bcat s ("  " ^ decoder_name ^",\n");
  bcat s (match flags with None -> "  ::flx::gc::generic::gc_flags_default,\n" | Some flags ->  flags^",\n");
  bcat s ("  0ul,0ul\n");
  bcat s ( "};\n")



