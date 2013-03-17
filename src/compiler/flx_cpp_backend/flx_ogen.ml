(* This module is responsible for generating shape tables for all dynamically allocated
 * objects
 *)

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


let comma_sub s =
  let rec aux l r =
    try   (* note: breaks tail recursion optimisation *)
      let i = String.index r ',' in
      let n = String.length r in
      aux (l ^ String.sub r 0 i ^ " comma ") (String.sub r (i+1) (n-i-1))
    with Not_found -> l ^ r
  in
  aux "" s
let id x = ()

let scan_bexpr syms bsym_table allocable_types e : unit =
  let rec aux e = match e with
    | BEXPR_new (_,t),_ when t <> Flx_btype.btyp_tuple [] ->
      (* print_endline ("FOUND A NEW " ^ sbt bsym_table t); *)
      let index =
        try Hashtbl.find syms.registry t
        with Not_found -> failwith ("new: Can't find type in registry " ^ sbt bsym_table t)
      in
      Hashtbl.replace allocable_types t index;

      | BEXPR_class_new (cls, _),_ ->
      (*
      print_endline ("FOUND A CLASS NEW " ^ sbt bsym_table t);
      *)
      let index =
        try Hashtbl.find syms.registry cls
        with Not_found -> failwith ("class new: Can't find type in registry " ^ sbt bsym_table cls)
      in
      Hashtbl.replace allocable_types cls index;

    | x -> ()
  in
  Flx_bexpr.iter ~f_bexpr:aux e

let scan_exe syms bsym_table allocable_types exe : unit =
  Flx_bexe.iter ~f_bexpr:(scan_bexpr syms bsym_table allocable_types) exe

let scan_exes syms bsym_table allocable_types exes : unit =
  iter (scan_exe syms bsym_table allocable_types) exes

let rec gen_type_shape s syms bsym_table need_int last_ptr_map primitive_shapes btyp index =
    print_debug syms ("allocable type --> " ^ sbt bsym_table btyp);
    let name = cpp_type_classname syms bsym_table btyp in
    if name = "int" then need_int := true else
    let btyp' = unfold btyp in 
    let gen_encoder () =
      let encoder_name = name ^ "_encoder" in
      let encoder_stmts = get_encoder' syms bsym_table "p" btyp' in
      bcat s ("\n// ENCODER for type " ^sbt bsym_table btyp' ^ "\n"); 
      bcat s (" ::std::string "^encoder_name^"(void *d) {\n");
      bcat s ("   char *p = (char*)d;\n");
      bcat s ("   ::std::string b = \"\";\n");
      iter (fun line -> bcat s ("   "^ line ^ "\n")) encoder_stmts;
      bcat s ("   return b;\n");
      bcat s (" }\n");
      encoder_name
    in
    let gen_decoder () =
      let decoder_name = name ^ "_decoder" in
      let decoder_stmts = get_decoder' syms bsym_table "p" btyp' in
      bcat s ("\n// DECODER for type " ^sbt bsym_table btyp' ^ "\n"); 
      bcat s ("  size_t "^decoder_name^"(void *d,char *s, size_t i) {\n");
      bcat s ("   char *p = (char*)d;\n");
      iter (fun line -> bcat s ("   "^ line ^ "\n")) decoder_stmts;
      bcat s ("   return i;\n");
      bcat s (" }\n");
      decoder_name
    in


    match btyp' with
    | BTYP_function _ -> ()

    | BTYP_tuple args ->
      let offsets = get_offsets syms bsym_table btyp in
      let n = length offsets in
      let encoder_name = gen_encoder () in
      let decoder_name = gen_decoder () in
      bcat s ("\n//OFFSETS for tuple type " ^ string_of_bid index ^ "\n");
      gen_offset_data s n name offsets false false [] None last_ptr_map encoder_name decoder_name

    (* This is a pointer, the offset data is in the system library *)
    | BTYP_pointer t -> ()

    (* for an array, we only have offsets for the first element *)
    | BTYP_array (t,i) ->
      let k =
        try Flx_btype.int_of_linear_type bsym_table i
        with Invalid_int_of_unitsum -> failwith "Array index must be unitsum"
      in
      let encoder_name = gen_encoder () in
      let decoder_name = gen_decoder () in
      let tname = cpp_typename syms bsym_table t in
      let offsets = get_offsets syms bsym_table t in
      let is_pod = is_pod bsym_table t in
      let n = length offsets in
      bcat s ("\n//OFFSETS for array type " ^ string_of_bid index ^ "\n");
      if n <> 0 then begin
        bcat s ("static ::std::size_t const " ^ name ^ "_offsets["^si n^"]={\n  ");
        bcat s ("  " ^ cat ",\n  " offsets);
        bcat s "};\n";
        bcat s ("static ::flx::gc::generic::offset_data_t const " ^name^"_offset_data = { " ^ 
          string_of_int n ^", " ^ name^ "_offsets};\n");
      end
      ;

      let this_ptr_map = name ^ "_ptr_map" in
      let old_ptr_map = !last_ptr_map in
      last_ptr_map := "&"^this_ptr_map;

      if not is_pod then begin
        bcat s ("static void " ^ name ^ "_finaliser(::flx::gc::generic::collector_t *, void *p){\n");
        bcat s ("  (("^ tname ^ "*)p)->~" ^ tname ^ "();\n");
        bcat s ("}\n")
      end
      ;
      bcat s ("static ::flx::gc::generic::gc_shape_t const "^ name ^"_ptr_map = {\n");
      bcat s ("  " ^ old_ptr_map ^ ",\n");
      bcat s ("  \"" ^ name ^ "\",\n");
      bcat s ("  " ^ si k ^ ",\n");
      bcat s ("  sizeof("^tname^"),\n"); (* NOTE: size of ONE element!! *)
      bcat s ( if not is_pod then ("  "^name^"_finaliser,\n") else ("  0,\n"));
      bcat s ("  "^ (if n<>0 then "&"^name^"_offset_data" else "0")^",\n");
      bcat s ("  "^ (if n<>0 then "&::flx::gc::generic::scan_by_offsets" else "0")^",\n");
      bcat s ("  "^encoder_name^",\n");
      bcat s ("  "^decoder_name^",\n");
      bcat s "  ::flx::gc::generic::gc_flags_default\n";
      bcat s "};\n"

    | BTYP_inst (i,ts) ->
      let bsym =
        try Flx_bsym_table.find bsym_table i
        with Not_found ->
          failwith (
            "[gen_offset_tables:BTYP_inst:allocable_types] can't find index " ^
            string_of_bid i)
      in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_external_type (_,quals,_,_) ->
        let complete = not (mem `Incomplete quals) in
        let is_pod = mem `Pod quals in
        let scanner = 
           try 
            List.iter (fun q-> match q with | `Scanner cs -> raise (Scanner cs) | _ -> () ) quals; 
            None 
          with Scanner cs -> Some cs 
        in
        let scanner = 
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


        if complete then
          if not (Hashtbl.mem primitive_shapes name) then
          begin
            let encoder_name = gen_encoder () in
            let decoder_name = gen_decoder () in
            Hashtbl.add primitive_shapes name true;
            bcat s ("\n//OFFSETS for complete abstract "^(if is_pod then "pod " else "finalisable ")^
              "type " ^ name ^ " instance "^
              (if scanner="0" then "" else "with custom scanner ")^
              (if is_pod || gen_dflt_finaliser then "" else "with custom finaliser ")^
              " \n"
            );

            let this_ptr_map = name ^ "_ptr_map" in
            let old_ptr_map = !last_ptr_map in
            last_ptr_map := "&"^this_ptr_map;

            if gen_dflt_finaliser then bcat s ("FLX_FINALISER("^name^")\n");
            bcat s ( "static ::flx::gc::generic::gc_shape_t const " ^ name ^ "_ptr_map = {\n") ;
            bcat s ("  " ^ old_ptr_map ^ ",\n");
            bcat s ("  \"" ^ name ^ "\",\n");
            bcat s ("  1,sizeof("^name^"),\n");
            bcat s ("  "^finaliser^","^(if is_pod then " // no finaliser" else "")^"\n");
            bcat s ("  0, // no client data\n");
            bcat s ("  "^scanner^", // scanner\n");
            bcat s ("  "^encoder_name^", // encoder\n");
            bcat s ("  "^decoder_name^", //decoder\n");
            bcat s ("  ::flx::gc::generic::gc_flags_default\n");
            bcat s "};\n"
          end else begin
            bcat s ("\n//OFFSETS for abstract type " ^ name ^ " instance\n");
            bcat s ("//Use "^name^"_ptr_map\n");
          end
        else
          clierr (Flx_bsym.sr bsym)
          ("[ogen] attempt to allocate an incomplete type: '" ^ Flx_bsym.id bsym ^"'")

      | BBDCL_union (vs,[id,n,t']) -> 
        let encoder_name = gen_encoder () in
        let decoder_name = gen_decoder () in
        print_endline "Warning VR_self rep not handled right?";
        let t'' = tsubst vs ts t' in
        gen_type_shape s syms bsym_table need_int last_ptr_map primitive_shapes t'' index
        
      | BBDCL_union _ -> () (* handled by universal uctor, int, etc *) 

      | BBDCL_cstruct (vs,cps, reqs) ->
        (* cstruct shouldn't have allocable stuff in it *)

        let this_ptr_map = name ^ "_ptr_map" in
        let old_ptr_map = !last_ptr_map in
        last_ptr_map := "&"^this_ptr_map;

        bcat s ("\n//OFFSETS for cstruct type " ^ name ^ " instance\n");

        (* HACK .. in fact, some C structs might have finalisers! *)
        let is_pod = false in
        if not is_pod then bcat s ("FLX_FINALISER("^name^")\n");
        bcat s ( "static ::flx::gc::generic::gc_shape_t const " ^ name ^ "_ptr_map ={\n") ;
        bcat s ("  " ^ old_ptr_map ^ ",\n");
        bcat s ("  \"" ^ name ^ "\",\n");
        if is_pod then begin
          bcat s ("  1,sizeof("^name^"),\n");
          bcat s ("  0,   // no finaliser\n");
          bcat s ("  0,0, // no client data or scanner\n");
          bcat s ("  0,0, // no encoder or decoder\n");
          bcat s ("  ::flx::gc::generic::gc_flags_default\n")
        end else begin
          bcat s ("  1,sizeof("^name^"),\n");
          bcat s ("  "^name^"_finaliser,\n");
          bcat s ("  0,0,   // no client data or scanner\n");
          bcat s ("  0,0, // no encoder or decoder\n");
          bcat s ("  ::flx::gc::generic::gc_flags_default\n")
        end
        ;
        bcat s "};\n"

      | BBDCL_struct (vs,cps) ->
        let encoder_name = gen_encoder () in
        let decoder_name = gen_decoder () in

        bcat s ("\n//OFFSETS for struct type " ^ name ^ " instance\n");
        let offsets = get_offsets syms bsym_table btyp in
        let n = length offsets in
        gen_offset_data s n name offsets false false [] None last_ptr_map encoder_name decoder_name

      | _ ->
        failwith
        (
          "[ogen]: can't handle instances of this kind yet: type " ^
          sbt bsym_table btyp
        )
    end

    | BTYP_record ("", es) ->
      let encoder_name = gen_encoder () in
      let decoder_name = gen_decoder () in
      bcat s ("\n//OFFSETS for record type " ^ name ^ " instance\n");
      let offsets = get_offsets syms bsym_table btyp in
      let n = length offsets in
      gen_offset_data s n name offsets false false [] None last_ptr_map encoder_name decoder_name
 
    | BTYP_unitsum _ ->
      bcat s ("static ::flx::gc::generic::gc_shape_t const &"^ name ^"_ptr_map = ::flx::rtl::_int_ptr_map;\n");

    | BTYP_sum _ ->
      begin match Flx_vrep.cal_variant_rep bsym_table btyp with
      | Flx_vrep.VR_self -> assert false
      | Flx_vrep.VR_int ->
        bcat s ("static ::flx::gc::generic::gc_shape_t const &"^ name ^"_ptr_map = ::flx::rtl::_int_ptr_map;\n");
      | Flx_vrep.VR_nullptr ->
        bcat s ("static ::flx::gc::generic::gc_shape_t const &"^ name ^"_ptr_map = ::flx::rtl::_address_ptr_map;\n");
      | Flx_vrep.VR_packed ->
        bcat s ("static ::flx::gc::generic::gc_shape_t const &"^ name ^"_ptr_map = ::flx::rtl::_address_ptr_map;\n");
      | Flx_vrep.VR_uctor ->
        bcat s ("static ::flx::gc::generic::gc_shape_t const &"^ name ^"_ptr_map = ::flx::rtl::_uctor_ptr_map;\n");
      end

    | _ ->
      failwith
      (
        "[ogen]: Unknown kind of allocable type " ^
        sbt bsym_table btyp
      )

let gen_offset_tables syms bsym_table module_name first_ptr_map=
  print_debug syms "GEN OFFSET TABLES";
  let allocable_types = Hashtbl.create 97 in
  let last_ptr_map = ref first_ptr_map in
  let primitive_shapes = Hashtbl.create 97 in
  let s = Buffer.create 20000 in
  
  (* Make a shape for every non-C style function with the property `Heap_closure *)
  print_debug syms "Make fun shapes";
  gen_all_fun_shapes (scan_exes syms bsym_table allocable_types) s syms bsym_table last_ptr_map;

  (* generate offsets for all pointers store in the thread_frame *)
  print_debug syms "Make thread frame offsets";
  gen_thread_frame_offsets s syms bsym_table last_ptr_map;

  (* We're not finished: we need offsets dynamically allocated types too *)

  (* currently the ONLY non-function types that can be allocated
    are the arguments of non-constant variant constructors:
    this WILL change when a 'new' operator is introduced.
  *)
  print_debug syms "Make shapes for dynamically allocated types";
  Hashtbl.iter
  (fun btyp index ->
print_debug syms ("Handle type " ^ sbt bsym_table btyp ^ " instance " ^ si index);
    match unfold btyp with
    | BTYP_sum args ->
      iter begin fun t ->
        match t with
        | BTYP_tuple []
        | BTYP_void -> ()
        | _ ->
          try
            let index = Hashtbl.find syms.registry t in
            Hashtbl.replace allocable_types t index
          with Not_found -> ()
      end args

    | BTYP_variant args ->
      iter begin fun (_,t) ->
        match t with
        | BTYP_tuple []
        | BTYP_void -> ()
        | _ ->
          try
            let index = Hashtbl.find syms.registry t in
            Hashtbl.replace allocable_types t index
          with Not_found -> ()
      end args

    | BTYP_inst (i,ts) ->
      print_debug syms ("Thinking about type index " ^ si i ^ " ts = " ^ catmap "," (sbt bsym_table) ts);
      let bsym =
        try Flx_bsym_table.find bsym_table i
        with Not_found ->
          failwith ("[gen_offset_tables:BTYP_inst] can't find index " ^
            string_of_bid i)
      in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_external_type (vs,bquals,_,_) ->
        (*
        print_endline ("abstract type "^Flx_bsym.id bsym ^ "["^catmap "," (sbt bsym_table) ts^"]");
        print_endline ("  properties: " ^ string_of_bquals bsym_table bquals);
        *)
        let handle_qual bqual = match bqual with
        | `Bound_needs_shape t ->
          (*
          print_endline ("Needs shape (uninstantiated) " ^ sbt bsym_table t);
          *)
          let varmap = mk_varmap vs ts in
          let t = varmap_subst varmap t in
          (*
          print_endline ("Needs shape (instantiated) " ^ sbt bsym_table t);
          *)
          begin try
            let index = Hashtbl.find syms.registry t in
            Hashtbl.replace allocable_types t index
          with
          | Not_found -> failwith ("[gen_offset_tables] Woops, type "^si i^ " = " ^ 
             sbt bsym_table t ^" isn't in registry! Required shape for " ^ bsym.Flx_bsym.id)
          end

        | _ -> ()
        in
        let rec aux quals = match quals with
        | [] -> ()
        | h :: t -> handle_qual h; aux t
        in aux bquals


      (* this routine assumes any use of a union component is
         allocable .. this is quite wrong but safe. This SHOULD
         be drived by detecting constructor expressions

         We don't need to worry about pattern matches .. if
         we didn't construct it, perhaps a foreigner did,
         in which case THEY needed to create the shape object
      *)
      | BBDCL_union (vs,args) ->
        let varmap = mk_varmap vs ts in
        let args = map (fun (_,_,t)->t) args in
        let args = map (varmap_subst varmap) args in
        iter begin fun t ->
          match t with
          | BTYP_tuple []
          | BTYP_void -> ()
          | _ ->
            try
              let index = Hashtbl.find syms.registry t in
              Hashtbl.replace allocable_types t index
            with Not_found -> ()
        end args

      | _ -> ()
      end
    | _ -> ()
  )
  syms.registry
  ;
  let need_int = ref false in
  Hashtbl.iter
  (fun btyp index -> gen_type_shape s syms bsym_table need_int last_ptr_map primitive_shapes btyp index 
  )
  allocable_types
  ;
  bcat s ("\n");
  if !need_int then
  bcat s ("static ::flx::gc::generic::gc_shape_t const &int_ptr_map = ::flx::rtl::_int_ptr_map;\n");

  bcat s ("// Head of shape list, included so dlsym() can find it when\n");
  bcat s ("// this file is a shared lib, uses module name for uniqueness.\n");
  bcat s ("extern \"C\" FLX_EXPORT ::flx::gc::generic::gc_shape_t const * const " ^ cid_of_flxid module_name ^ "_head_shape;\n");
  bcat s ("::flx::gc::generic::gc_shape_t const * const " ^ cid_of_flxid module_name ^ "_head_shape=" ^ !last_ptr_map ^ ";\n");
  !last_ptr_map,Buffer.contents s
