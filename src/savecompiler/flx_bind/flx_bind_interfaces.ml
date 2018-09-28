open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bbdcl
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_unify
open Flx_exceptions
open List
open Flx_generic
open Flx_tpat
open Flx_name_map
open Flx_bid
open Flx_bind_reqs
open Flx_bbind_state


let bind_interface (state:bbind_state_t) bsym_table = function
  | sr, IFACE_export_fun (sn, cpp_name), parent ->
      let env = Flx_lookup.build_env state.lookup_state bsym_table parent in
      let index,ts = Flx_lookup.lookup_sn_in_env
        state.lookup_state
        bsym_table
        env
        sn
      in
      if ts = [] then
        BIFACE_export_fun (sr,index, cpp_name)
      else clierrx "[flx_bind/flx_bbind.ml:944: E3] " sr
      (
        "Can't export generic entity " ^
        string_of_suffixed_name sn ^ " as a function"
      )

  | sr, IFACE_export_cfun (sn, cpp_name), parent ->
      let env = Flx_lookup.build_env state.lookup_state bsym_table parent in
      let index,ts = Flx_lookup.lookup_sn_in_env
        state.lookup_state
        bsym_table
        env
        sn
      in
      if ts = [] then
        BIFACE_export_cfun (sr,index, cpp_name)
      else clierrx "[flx_bind/flx_bbind.ml:960: E4] " sr
      (
        "Can't export generic entity " ^
        string_of_suffixed_name sn ^ " as a C function"
      )

  | sr, IFACE_export_python_fun (sn, cpp_name), parent ->
      let env = Flx_lookup.build_env state.lookup_state bsym_table parent in
      let index,ts =
        Flx_lookup.lookup_sn_in_env
        state.lookup_state
        bsym_table
        env
        sn
      in
      if ts = [] then
        BIFACE_export_python_fun (sr,index, cpp_name)
      else clierrx "[flx_bind/flx_bbind.ml:977: E5] " sr
      (
        "Can't export generic entity " ^
        string_of_suffixed_name sn ^ " as a python function"
      )

  | sr, IFACE_export_type (typ, cpp_name), parent ->
      let env = Flx_lookup.build_env state.lookup_state bsym_table parent in
      let t = Flx_lookup.bind_type
        state.lookup_state
        bsym_table
        env
        Flx_srcref.dummy_sr
        typ
      in
      if try Flx_btype_occurs.var_occurs bsym_table t with _ -> true then
      clierrx "[flx_bind/flx_bbind.ml:993: E6] " sr
      (
        "Can't export generic- or meta- type " ^
        sbt bsym_table t
      )
      else
        BIFACE_export_type (sr, t, cpp_name)

  | sr, IFACE_export_struct (name), parent ->
      let env = Flx_lookup.build_env state.lookup_state bsym_table parent in
      let entry_set  = Flx_lookup.lookup_name_in_env state.lookup_state bsym_table env sr name in
      begin match entry_set with
      | FunctionEntry _ -> assert false
      | NonFunctionEntry  {base_sym = index; spec_vs = vs; sub_ts = ts} ->
        begin match  vs, ts with [],[] -> () | _ -> assert false end;
        let bbdcl = Flx_bsym_table.find_bbdcl bsym_table index in
        begin match bbdcl with
        | BBDCL_struct _ -> BIFACE_export_struct (sr,index)
        | _ ->
          clierrx "[flx_bind/flx_bbind.ml:1012: E7] " sr ("Attempt to export struct "^name^
          " which isn't a non-polymorphic struct, got entry : " ^ 
          Flx_print.string_of_bbdcl bsym_table bbdcl index)
        end
     end

  | sr, IFACE_export_union (flx_name, cpp_name), parent ->
      let env = Flx_lookup.build_env state.lookup_state bsym_table parent in
      let index,ts = Flx_lookup.lookup_sn_in_env
        state.lookup_state
        bsym_table
        env
        flx_name 
      in
      if ts = [] then
        BIFACE_export_union (sr,index, cpp_name)
      else clierrx "[flx_bind/flx_bbind.ml:1028: E8] " sr
      (
        "Can't export generic union " ^
        string_of_suffixed_name flx_name ^ " as C datatype"
      )

  | sr, IFACE_export_requirement (reqs), parent ->
      let env = Flx_lookup.build_env state.lookup_state bsym_table parent in
      let bt t = Flx_lookup.bind_type state.lookup_state bsym_table env sr t in
      let breqs = bind_reqs bt state bsym_table env sr reqs in
      BIFACE_export_requirement (sr,breqs)



