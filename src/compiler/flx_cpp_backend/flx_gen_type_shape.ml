(* This module is responsible for generating shape for one type *)

(* Convenience function for printing debug statements. *)
let print_debug syms msg =
  if syms.Flx_mtypes2.compiler_options.Flx_options.print_flag
  then print_endline msg


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
open Flx_cal_fun_offsets
open Flx_cal_threadframe_offsets
open Flx_findvars
open Flx_gen_shape
open Flx_btype_subst


module CS = Flx_code_spec
exception Scanner of CS.t

let rec gen_type_shape module_name s h syms bsym_table need_int primitive_shapes btyp index new_table =
    print_debug syms ("allocable type --> " ^ sbt bsym_table btyp);
    let name = cpp_type_classname syms bsym_table btyp in
    if name = "int" then need_int := true else
    let btyp' = unfold "flx_ogen: gen_type_shape" btyp in 
    let gen_encoder () =
      let encoder_name = name ^ "_encoder" in
      let encoder_stmts = Flx_codec.get_encoder' syms bsym_table "p" btyp' in
      bcat s ("\n// ENCODER for type " ^sbt bsym_table btyp' ^ "\n"); 
      bcat s ("  ::std::string "^encoder_name^"(void *d) {\n");
      bcat s ("    char *p = (char*)d;\n");
      bcat s ("    ::std::string b = \"\";\n");
      iter (fun line -> bcat s ("   "^ line ^ "\n")) encoder_stmts;
      bcat s ("    return b;\n");
      bcat s ("  }\n");
      encoder_name
    in
    let gen_decoder () =
      let decoder_name = name ^ "_decoder" in
      let decoder_stmts = Flx_codec.get_decoder' syms bsym_table "p" btyp' in
      bcat s ("\n// DECODER for type " ^sbt bsym_table btyp' ^ "\n"); 
      bcat s ("  size_t "^decoder_name^"(void *d,char *s, size_t i) {\n");
      bcat s ("    char *p = (char*)d;\n");
      iter (fun line -> bcat s ("   "^ line ^ "\n")) decoder_stmts;
      bcat s ("    return i;\n");
      bcat s ("  }\n");
      decoder_name
    in

   let gen_first_class () = 
      let o_name = name ^ "_fcops" in
      bcat s ("\nCxxValueType<"^name^"> " ^o_name ^ ";\n"); 
      o_name
   in
      
    match btyp' with
    | _ when islinear_type btyp' -> ()

    | BTYP_function _ -> ()

    | BTYP_tuple args ->
      let offsets = get_offsets' syms bsym_table btyp in
      let n = length offsets in
      let encoder_name = gen_encoder () in
      let decoder_name = gen_decoder () in
      let oname  = "&" ^ (gen_first_class ()) in
      bcat s ("\n//**************************************\n");
      bcat s ("//SHAPE for tuple type " ^ string_of_bid index ^ "\n");
      gen_offset_data syms bsym_table module_name s h n name offsets false false [] None encoder_name decoder_name new_table

    (* This is a pointer, the offset data is in the system library *)
    | BTYP_ptr (_,t,[]) -> ()

    (* for an array, we only have offsets for the first element *)
    | BTYP_array (t,i) ->
      let k =
        try Flx_btype.int_of_linear_type bsym_table i
        with Invalid_int_of_unitsum -> failwith "Array index must be unitsum"
      in
      let encoder_name = gen_encoder () in
      let decoder_name = gen_decoder () in
      let tname = cpp_typename syms bsym_table t in
      let offsets = get_offsets' syms bsym_table t in
      let is_pod = Flx_pod.is_pod bsym_table t in
      let n = length offsets in
      bcat s ("\n//**************************************\n");
      bcat s ("//SHAPE for array type " ^ string_of_bid index ^ "\n");
      if n <> 0 then begin
        bcat s ("static ::flx::gc::generic::offset_entry_t const " ^ name ^ "_offsets["^si n^"]={\n  ");
        bcat s ("  " ^ catmap ",\n  " (render_offset syms bsym_table new_table) offsets);
        bcat s "};\n";
        bcat s ("static ::flx::gc::generic::offset_data_t const " ^name^"_offset_data = { " ^ 
          string_of_int n ^", " ^ name^ "_offsets};\n");
      end
      ;

      let this_ptr_map = name ^ "_ptr_map" in

      if not is_pod then begin
        bcat s ("static void " ^ name ^ "_finaliser(::flx::gc::generic::collector_t *, void *p){\n");
        bcat s ("  (("^ tname ^ "*)p)->~" ^ tname ^ "();\n");
        bcat s ("}\n")
      end
      ;
      bcat h ("extern ::flx::gc::generic::gc_shape_t "^ name ^"_ptr_map;\n");
      bcat s ("extern ::flx::gc::generic::gc_shape_t "^ name ^"_ptr_map = {\n");
      bcat s ("  \"" ^ module_name ^ "::"^ name ^ "\",\n");
      bcat s ("  " ^ si k ^ ",\n");
      bcat s ("  sizeof("^tname^"),\n"); (* NOTE: size of ONE element!! *)
      bcat s ( if not is_pod then ("  "^name^"_finaliser,\n") else ("  0,\n"));
      bcat s ("  0, // fcops\n");
      bcat s ("  "^ (if n<>0 then "&"^name^"_offset_data" else "0")^",\n");
      bcat s ("  "^ (if n<>0 then "&::flx::gc::generic::scan_by_offsets" else "0")^",\n");
      bcat s ("  "^encoder_name^",\n");
      bcat s ("  "^decoder_name^",\n");
      bcat s "  ::flx::gc::generic::gc_flags_default,0ul,0ul\n";
      bcat s "};\n"

    | BTYP_inst (`Nominal, i,ts,_) ->
(*
print_endline ("NOMINAL TYPE " ^ sbt bsym_table btyp);
*)
      let bsym =
        try Flx_bsym_table.find bsym_table i
        with Not_found ->
          failwith (
            "[gen_offset_tables:BTYP_inst:allocable_types] can't find index " ^
            string_of_bid i)
      in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_external_type (_,quals,_,_) ->
        let is_functor = mem (`TypeTag "functor") quals && List.length ts > 0 in 
        let complete = not (mem `Incomplete quals) in
        let copyable = not (mem `Uncopyable quals) && complete in
        let is_pod = mem `Pod quals in
        let gc_pointer = mem `GC_pointer quals in 
        let scanner = 
           try 
            List.iter (fun q-> match q with | `Scanner cs -> raise (Scanner cs) | _ -> () ) quals; 
            None 
          with Scanner cs -> Some cs 
        in
        let scanner = 
          if is_functor && List.length ts > 0 then 
            "::flx::gc::generic::stl_container_scanner<" ^ name ^ ">"
          else if gc_pointer then 
            "&::flx::gc::generic::scan_by_offsets" 
          else
          match scanner with 
          | None -> "0" 
          | Some (CS.Str s) -> s
          | Some (CS.Str_template s) -> s
          | Some _ -> assert false
        in

        let finaliser = 
           try 
            List.iter (fun q-> match q with | `Finaliser cs -> raise (Scanner cs) | _ -> () ) quals; 
            None 
          with Scanner cs -> Some cs 
        in
        let finaliser = 
          match finaliser with 
          | None -> "0" 
          | Some (CS.Str s) -> s
          | Some (CS.Str_template s) -> s
          | Some _ -> assert false
        in
        let finaliser, gen_dflt_finaliser =
          if finaliser = "0"
          then 
            if is_pod then "0",false
            else name ^ "_finaliser",true
          else finaliser, false
        in

        bcat s ("\n//**************************************\n");
        bcat s ("//ABSTRACT TYPE " ^ name ^"\n");
        if complete then
          let oname =
            if copyable then "&" ^ gen_first_class () else "0"
          in
          if not (Hashtbl.mem primitive_shapes name) then
          begin
            let encoder_name = gen_encoder () in
            let decoder_name = gen_decoder () in
            Hashtbl.add primitive_shapes name true;
            bcat s ("\n//**************************************\n");
            bcat s ("//SHAPE for complete abstract "^(if is_pod then "pod " else "finalisable ")^
              (if gc_pointer then "GC pointer " else "") ^ "type " ^ name ^ "="^sbt bsym_table btyp' ^ " instance "^
              (if scanner="0" then "" else "with custom scanner ")^
              (if is_pod || gen_dflt_finaliser then "" else "with custom finaliser ")^
              " \n"
            );
            let fmap_name =  name ^ "_value_type_shapes_index_"^si i^"_instance_"^si index in
            if is_functor then begin
              let shape t = 
                Flx_pgen.shape_of' true syms bsym_table (cpp_typename syms bsym_table) t
              in
              let tarray = List.map (fun t -> "&" ^ shape t) ts in
              bcat s ("//FUNCTOR, arg types = " ^  catmap "," (sbt bsym_table) ts^ "\n");
              bcat s ("static ::flx::gc::generic::gc_shape_t *"^fmap_name^"[" ^ si (List.length ts) ^ "]={"^ String.concat ", " tarray^"};\n");
              List.iter (fun t -> 
                let index' = Flx_treg.find_type_index syms bsym_table t in
                Hashtbl.replace new_table t index'
              ) ts
            end;
              
            let this_ptr_map = name ^ "_ptr_map" in

            if gen_dflt_finaliser then bcat s ("FLX_FINALISER("^name^")\n");
            bcat h ( "extern ::flx::gc::generic::gc_shape_t " ^ name ^ "_ptr_map;\n") ;
            bcat s ( "extern ::flx::gc::generic::gc_shape_t " ^ name ^ "_ptr_map = {\n") ;
            bcat s ("  \"" ^ module_name ^"::" ^ name ^ "\",\n");
            bcat s ("  1,sizeof("^name^"),\n");
            bcat s ("  "^finaliser^","^(if is_pod then " // no finaliser" else "")^"\n");
            bcat s ("  "^oname^",\n");
            begin if gc_pointer then 
              bcat s ("  &::flx::rtl::_address_offset_data, // gc_pointer\n")
            else if is_functor then
              bcat s ("  " ^ fmap_name ^",\n")
            else
              bcat s ("  0, // no client data\n")
            end;
            bcat s ("  "^scanner^", // scanner\n");
            bcat s ("  "^encoder_name^", // encoder\n");
            bcat s ("  "^decoder_name^", //decoder\n");
            bcat s ("  ::flx::gc::generic::gc_flags_default,0ul,0ul\n");
            bcat s "};\n"
          end else begin
            bcat s ("\n//**************************************\n");
            bcat s ("//SHAPE for abstract type " ^ name ^ " instance\n");
            bcat s ("//Use "^name^"_ptr_map\n");
          end
        else
          clierrx "[flx_cpp_backend/flx_ogen.ml:285: E311] " (Flx_bsym.sr bsym)
          ("[ogen] attempt to allocate an incomplete type: '" ^ Flx_bsym.id bsym ^"'")

      | BBDCL_union (vs,[id,n,[],t',_,_]) -> 
print_endline ("[flx_ogen] One component union should have been removed");
print_endline ("\n//One component union TYPE " ^ name ^" ctor name = "^id^
" index=" ^ si n^ 
", argtype = "^
 sbt bsym_table t' ^ "\n");
bcat s ("\n//UNION TYPE " ^ name ^"\n");
        let encoder_name = gen_encoder () in
        let decoder_name = gen_decoder () in
        print_endline "Warning VR_self rep not handled right?";
        let t'' = tsubst (Flx_bsym.sr bsym) vs ts t' in
        gen_type_shape module_name s h syms bsym_table need_int primitive_shapes t'' index new_table
        
      | BBDCL_union _ -> () (* handled by universal uctor, int, etc *) 

      | BBDCL_cstruct (vs,cps, reqs) ->
        (* cstruct shouldn't have allocable stuff in it *)

        let this_ptr_map = name ^ "_ptr_map" in

        bcat s ("\n//**************************************\n");
        bcat s ("//SHAPE for cstruct type " ^ name ^ " instance\n");

        (* HACK .. in fact, some C structs might have finalisers! *)
        let is_pod = false in
        if not is_pod then bcat s ("FLX_FINALISER("^name^")\n");
        bcat h ( "extern ::flx::gc::generic::gc_shape_t " ^ name ^ "_ptr_map;\n") ;
        bcat s ( "extern ::flx::gc::generic::gc_shape_t " ^ name ^ "_ptr_map ={\n") ;
        bcat s ("  \"" ^ module_name ^ "::" ^ name ^ "\",\n");
        if is_pod then begin
          bcat s ("  1,sizeof("^name^"),\n");
          bcat s ("  0,   // no finaliser\n");
          bcat s ("  0,  // fcops\n");
          bcat s ("  0,0, // no client data or scanner\n");
          bcat s ("  0,0, // no encoder or decoder\n");
          bcat s ("  ::flx::gc::generic::gc_flags_default,0ul,0ul\n")
        end else begin
          bcat s ("  1,sizeof("^name^"),\n");
          bcat s ("  "^name^"_finaliser,\n");
          bcat s ("  0, // fcops\n");
          bcat s ("  0,0,   // no client data or scanner\n");
          bcat s ("  0,0, // no encoder or decoder\n");
          bcat s ("  ::flx::gc::generic::gc_flags_default,0ul,0ul\n")
        end
        ;
        bcat s "};\n"

      | BBDCL_struct (vs,cps) ->
        let encoder_name = gen_encoder () in
        let decoder_name = gen_decoder () in
        let oname = "&" ^ ( gen_first_class ()) in

        bcat s ("\n//**************************************\n");
        bcat s ("//SHAPE for struct type " ^ name ^ " instance\n");
        let offsets = get_offsets' syms bsym_table btyp in
        let n = length offsets in
        gen_offset_data syms bsym_table module_name s h n name offsets false false [] None encoder_name decoder_name new_table

      | _ ->
        failwith
        (
          "[ogen]: can't handle instances of this kind yet: type " ^
          sbt bsym_table btyp
        )
    end

    | BTYP_record (es) ->
      let encoder_name = gen_encoder () in
      let decoder_name = gen_decoder () in
      let oname = "&" ^ (gen_first_class ()) in
      bcat s ("\n//**************************************\n");
      bcat s ("//SHAPE for record type " ^ name ^ " instance\n");
      let offsets = get_offsets' syms bsym_table btyp in
      let n = length offsets in
      gen_offset_data syms bsym_table module_name s h n name offsets false false [] None encoder_name decoder_name new_table
 
    | BTYP_rptsum _
    | BTYP_sum _ ->
      begin match Flx_vrep.cal_variant_rep bsym_table btyp with
      | Flx_vrep.VR_self -> assert false
      | Flx_vrep.VR_clt ->
        bcat s ("static ::flx::gc::generic::gc_shape_t &"^ name ^"_ptr_map = ::flx::rtl::_int_ptr_map;\n");
      | Flx_vrep.VR_int ->
        bcat s ("static ::flx::gc::generic::gc_shape_t &"^ name ^"_ptr_map = ::flx::rtl::_int_ptr_map;\n");
      | Flx_vrep.VR_nullptr ->
        bcat s ("static ::flx::gc::generic::gc_shape_t &"^ name ^"_ptr_map = ::flx::rtl::_address_ptr_map;\n");
      | Flx_vrep.VR_packed ->
        bcat s ("static ::flx::gc::generic::gc_shape_t &"^ name ^"_ptr_map = ::flx::rtl::_address_ptr_map;\n");
      | Flx_vrep.VR_uctor ->
        bcat s ("static ::flx::gc::generic::gc_shape_t &"^ name ^"_ptr_map = ::flx::rtl::_uctor_ptr_map;\n");
      end

    | BTYP_variant _ -> ()

    | _ ->
      failwith
      (
        "[ogen]: Unknown kind of allocable type " ^
        sbt bsym_table btyp
      )


