open List

open Flx_bbdcl
open Flx_beta
open Flx_bexe
open Flx_bexpr
open Flx_bparameter
open Flx_btype
open Flx_cexpr
open Flx_ctorgen
open Flx_ctypes
open Flx_display
open Flx_egen
open Flx_exceptions
open Flx_label
open Flx_list
open Flx_maps
open Flx_mtypes2
open Flx_name
open Flx_ogen
open Flx_options
open Flx_pgen
open Flx_print
open Flx_types
open Flx_typing
open Flx_unify
open Flx_util
open Flx_gen_helper

let gen_fun_header syms bsym_table kind index export_name modulename =
    let mname = Flx_name.cid_of_flxid modulename in
    let bsym =
      try Flx_bsym_table.find bsym_table index with Not_found ->
        failwith ("[gen_biface_header] Can't find index " ^ string_of_bid index)
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,ps,ret,effects,_) ->
      let display = get_display_list bsym_table index in
      if length display <> 0
      then clierrx "[flx_cpp_backend/flx_gen_biface.ml:40: E290] " (Flx_bsym.sr bsym) ("Can't export nested function " ^ export_name);

      let arglist =
        List.map
        (fun {ptyp=t} -> cpp_typename syms bsym_table t)
        (Flx_bparams.get_params ps)
      in
      let arglist = "  " ^
        match kind with 
        | `Fun -> 
           (if length arglist = 0 then "FLX_FPAR_DECL_ONLY"
           else "FLX_FPAR_DECL\n" ^ cat ",\n  " arglist
           )
        | `Cfun -> cat ",\n  " arglist
      in
      let name, rettypename =
        match ret with
        | BTYP_void -> "PROCEDURE", "::flx::rtl::con_t * "
        | _ -> "FUNCTION", cpp_typename syms bsym_table ret
      in

      "//EXPORT " ^ name ^ " " ^ cpp_instance_name syms bsym_table index [] ^
      " as " ^ export_name ^ "\n" ^
      "extern \"C\" {\n" ^
      "  using namespace ::flxusr::" ^ mname ^";\n" ^
      "  FLX_EXTERN_"^mname ^ " " ^rettypename ^ " " ^
      export_name ^ "(\n" ^ arglist ^ "\n);\n}\n"

    | _ -> failwith "Not implemented: export non-function/procedure"
    end


let gen_biface_header syms bsym_table modulename biface = 
  let mname = Flx_name.cid_of_flxid modulename in
  match biface with
  | BIFACE_export_python_fun (sr,index, export_name) ->
     "// PYTHON FUNCTION " ^ export_name ^ " header to go here??\n"

  | BIFACE_export_fun (sr,index, export_name) ->
    gen_fun_header syms bsym_table `Fun index export_name modulename

  | BIFACE_export_cfun (sr,index, export_name) ->
    gen_fun_header syms bsym_table `Cfun index export_name modulename

  | BIFACE_export_type (sr, typ, export_name) ->
    "//EXPORT type " ^ sbt bsym_table typ ^ " as " ^ export_name  ^ "\n" ^
    (* "typedef ::flxusr::" ^ mname ^ "::" ^ cpp_type_classname syms bsym_table typ ^ " " ^ export_name ^ "_class;\n" ^ *)

    "namespace flxusr { namespace " ^ mname ^ "{\n" ^
    "  using " ^ export_name ^ " = " ^ cpp_typename syms bsym_table typ ^ "; // C++11\n" ^
    "}}\n" ^
    "typedef ::flxusr::" ^  mname ^ "::" ^ export_name ^ " " ^ export_name ^ ";\n"

  | BIFACE_export_union (sr, idx, export_name) ->
    let typ = Flx_btype.btyp_inst (idx,[],Flx_kind.KIND_type) in 
    let sym = Flx_bsym_table.find bsym_table idx in 
    let fname = sym.Flx_bsym.id in
    "//EXPORT union "  ^ fname ^ ", type " ^ sbt bsym_table typ ^ " as " ^ export_name  ^ "\n" ^
    "namespace flxusr { namespace " ^ mname ^ "{\n" ^
    "  using " ^ export_name ^ " = " ^ cpp_typename syms bsym_table typ ^ "; // C++11\n" ^
    "}}\n" ^
    "typedef ::flxusr::" ^  mname ^ "::" ^ export_name ^ " " ^ export_name ^ ";\n"


  | BIFACE_export_struct (sr,idx) ->
    let bsym = Flx_bsym_table.find bsym_table idx in
    let sname = Flx_bsym.id bsym in
    let bbdcl = Flx_bsym.bbdcl bsym in
    let typ = Flx_btype.btyp_inst (idx,[],Flx_kind.KIND_type) in
    let classname = cpp_type_classname syms bsym_table typ in
    "//EXPORT struct " ^ sname ^ "\n" ^
    "typedef ::flxusr::" ^ mname ^ "::" ^ classname ^ " " ^ sname ^ ";\n"

  | BIFACE_export_requirement (sr,breqs) -> ""

let gen_fun_body syms bsym_table (shapes: Flx_set.StringSet.t ref) shape_map
    label_map kind index export_name 
=
    let bsym =
      try Flx_bsym_table.find bsym_table index with Not_found ->
        failwith ("[gen_biface_body] Can't find index " ^ string_of_bid index)
    in
    let sr = Flx_bsym.sr bsym in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,ps,BTYP_void,effects,_) ->
      if length vs <> 0
      then clierrx "[flx_cpp_backend/flx_gen_biface.ml:126: E291] " sr ("Can't export generic procedure " ^ Flx_bsym.id bsym)
      ;
      let display = get_display_list bsym_table index in
      if length display <> 0
      then clierrx "[flx_cpp_backend/flx_gen_biface.ml:130: E292] " (Flx_bsym.sr bsym) "Can't export nested function";

      let args = rev (fold_left (fun args
        ({ptyp=t; pid=name; pindex=pidx} as arg) ->
        try ignore(cpp_instance_name syms bsym_table pidx []); arg :: args
        with _ -> args
        )
        []
        (Flx_bparams.get_params ps))
      in
      let params =
        List.map
        (fun {ptyp=t; pindex=pidx; pid=name} ->
          cpp_typename syms bsym_table t ^ " " ^ name
        )
        (Flx_bparams.get_params ps)
      in
      let strparams = "  " ^
        match kind with
        | `Fun ->
          (if length params = 0 then "FLX_FPAR_DECL_ONLY"
          else "FLX_FPAR_DECL\n  " ^ cat ",\n  " params
          )
        | `Cfun -> cat ",\n " params
      in
      let class_name = cpp_instance_name syms bsym_table index [] in
      let strargs =
        let ge = gen_expr syms bsym_table shapes shape_map label_map index [] [] in
        match Flx_bparams.get_params ps with
        | [] -> "0"
        | [{ptyp=t; pid=name; pindex=idx}] -> "0" ^ ", " ^ name
        | _ ->
          let a =
            let counter = ref 0 in
            bexpr_tuple
              (btyp_tuple (Flx_bparams.get_btypes ps))
              (
                List.map
                (fun {ptyp=t; pid=name; pindex=idx} ->
                  bexpr_expr (Flx_code_spec.Str name,t,bexpr_unit)
                )
                (Flx_bparams.get_params ps)
              )
          in
          "0" ^ ", " ^ ge sr a
      in
      let call_method = 
         if mem `Cfun props || mem `Pure props && mem `Stackable props then `C_call
         else if mem `Stackable props then `Stack_call
         else if mem `Heap_closure props then `Heap_call
         else 
           let bug = 
             "Function exported as " ^ export_name ^ " is neither stackable " ^
             " nor has a heap closure -- no way to call it" 
           in clierrx "[flx_cpp_backend/flx_gen_biface.ml:184: E293] " sr bug
      in
      let requires_ptf = mem `Requires_ptf props in


      "//EXPORT PROC " ^ cpp_instance_name syms bsym_table index [] ^
      " as " ^ export_name ^ "\n" ^
      "::flx::rtl::con_t *" ^ export_name ^ "(\n" ^ strparams ^ "\n){\n" ^
      ( 
        match call_method with
        | `C_call ->
          "  " ^ class_name ^"(" ^
          (
            if requires_ptf then
            begin match kind with
            | `Fun ->
              if length args = 0
              then "FLX_APAR_PASS_ONLY "
              else "FLX_APAR_PASS "
            | `Cfun -> 
              clierrx "[flx_cpp_backend/flx_gen_biface.ml:204: E294] " (Flx_bsym.sr bsym) ("Attempt to export procedure requiring thread frame with C interface: "^ Flx_bsym.id bsym)
            end
            else ""
          )
          ^
          catmap  ", " (fun {pid=id}->id) args ^ ");\n" ^
          "  return 0;\n"

        | `Stack_call ->
          "  " ^ class_name ^ "("^
          (
            if requires_ptf then
            begin match kind with
            | `Fun -> "_PTFV"
            | `Cfun -> 
              clierrx "[flx_cpp_backend/flx_gen_biface.ml:219: E295] " (Flx_bsym.sr bsym) ("Attempt to export procedure requiring thread frame with C interface: "^ Flx_bsym.id bsym)
            end
            else ""
          )
          ^
          ")" ^
          ".stack_call(" ^ (catmap ", " (fun {pid=id}->id) args) ^ ");\n"
          ^
          "  return 0;\n"
        | `Heap_call ->
          "  return (new(*_PTF gcp,"^class_name^"_ptr_map,true)\n" ^
          "    " ^ class_name ^ "(_PTFV))" ^
          "\n      ->call(" ^ strargs ^ ");\n"
      )
      ^
      "}\n"

    | BBDCL_fun (props,vs,ps,ret,effects,_) ->
      if length vs <> 0
      then clierrx "[flx_cpp_backend/flx_gen_biface.ml:238: E296] " (Flx_bsym.sr bsym) ("Can't export generic function " ^ Flx_bsym.id bsym)
      ;
      let display = get_display_list bsym_table index in
      if length display <> 0
      then clierrx "[flx_cpp_backend/flx_gen_biface.ml:242: E297] " sr "Can't export nested function";
      let arglist =
        List.map
        (fun {ptyp=t; pid=name} -> cpp_typename syms bsym_table t ^ " " ^ name)
        (Flx_bparams.get_params ps)
      in
      let arglist = "  " ^
        match kind with
        | `Fun ->
          (if length arglist = 0 then "FLX_FPAR_DECL_ONLY"
          else "FLX_FPAR_DECL\n  " ^ cat ",\n  " arglist
          )
        | `Cfun ->  cat ",\n " arglist
      in
(*
print_endline ("Export " ^ export_name ^ " properties " ^ string_of_properties props);
      if mem `Stackable props then print_endline ("Stackable " ^ export_name);
      if mem `Stack_closure props then print_endline ("Stack_closure" ^ export_name);
      if mem `Heap_closure props then print_endline ("Heap_closure" ^ export_name);
*)
      let call_method = 
         if mem `Pure props && mem `Stackable props then `C_call
         else if mem `Stackable props then `Stack_call
         else if mem `Heap_closure props then `Heap_call
         else 
           let bug = 
             "Function exported as " ^ export_name ^ " is neither stackable " ^
             " nor has a heap closure -- no way to call it" 
           in clierrx "[flx_cpp_backend/flx_gen_biface.ml:270: E298] " sr bug
      in
      let requires_ptf = mem `Requires_ptf props in

      let rettypename = cpp_typename syms bsym_table ret in
      let class_name = cpp_instance_name syms bsym_table index [] in
      let ge = gen_expr syms bsym_table shapes shape_map label_map index [] [] in

      let args =
        match fst ps with
        | Flx_ast.Slist [] -> "" (* should only occur at top level *)
        | Flx_ast.Satom {pid=name;ptyp=t} -> name
        | _ -> 
          (* this mess is because (a,(b,c)) for example isn't a tuple in C,
             so to call the Felix function "as if" the above were a tuple
             we have to construct an actual tuple
          *)
          let rec aux ps = match ps with
          | Flx_ast.Satom {pid=name;ptyp=t} -> bexpr_expr (Flx_code_spec.Str name,t, bexpr_unit)
          | Flx_ast.Slist pss  ->
            bexpr_tuple
              (Flx_bparams.xget_btype ps)
              (List.map aux pss)
          in
          let a = aux (fst ps) in
          ge sr a
      in


      "//EXPORT FUNCTION " ^ class_name ^
      " as " ^ export_name ^ "\n" ^
      rettypename ^" " ^ export_name ^ "(\n" ^ arglist ^ "\n){\n" ^
      begin match call_method with
      | `C_call -> 
        let names = Flx_bparams.get_names ps in
        "  return " ^ class_name ^ "("^
        (if requires_ptf then "_PTFV"^(if List.length names > 0 then ", " else "") else "") ^
        cat ", " names ^ ");\n"

      | `Stack_call ->
        "  return " ^ class_name ^ "("^
          (if requires_ptf then "_PTFV" else "") ^").apply(" ^
          args^ ");\n"
      | `Heap_call ->
        "  return (new(*_PTF gcp,"^class_name^"_ptr_map,true)\n" ^
        "    " ^ class_name ^ "(_PTFV))\n" ^
        "    ->apply(" ^ args ^ ");\n"
      end 
      ^
      "}\n"

    | _ -> failwith "Not implemented: export non-function/procedure"
    end

let gen_biface_body syms bsym_table shapes shape_map label_map biface = match biface with
  | BIFACE_export_python_fun (sr,index, export_name) ->
     "// PYTHON FUNCTION " ^ export_name ^ " body to go here??\n"

  | BIFACE_export_fun (sr,index, export_name) ->
    gen_fun_body syms bsym_table shapes shape_map label_map `Fun index export_name

  | BIFACE_export_cfun (sr,index, export_name) ->
    gen_fun_body syms bsym_table shapes shape_map label_map `Cfun index export_name

  | BIFACE_export_type _ -> ""

  | BIFACE_export_struct _ -> ""

  | BIFACE_export_union _ -> ""

  | BIFACE_export_requirement _ -> ""

let gen_felix_binding syms bsym_table kind index export_name modulename =
  let mname = Flx_name.cid_of_flxid modulename in
  let bsym =
    try Flx_bsym_table.find bsym_table index with Not_found ->
      failwith ("[gen_biface_header] Can't find index " ^ string_of_bid index)
  in
  begin match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,origps,ret,effects,_) ->
    let display = get_display_list bsym_table index in
    if length display <> 0
    then clierrx "[flx_cpp_backend/flx_gen_biface.ml:349: E299] " (Flx_bsym.sr bsym) ("Can't export nested function " ^ export_name);

    (* THIS BIT FOR DOCO ONLY *)
    (* FIXME: HACK, probably not right, just get it to compile *)
    let ps = Flx_bparams.get_params origps in 
    let n = List.length ps in
    let fkind, rettypename =
      match ret with
      | BTYP_void -> "proc", "::flx::rtl::con_t * "
      | _ -> "fun", cpp_typename syms bsym_table ret
    in
    let fkind = match kind with `Fun -> fkind | `Cfun -> "c" ^ fkind in
    (* THIS IS THE FELIX INTERFACE *)
    let fname = Flx_bsym.id bsym in
    let farglist = 
      List.map (fun {ptyp=t} -> sbt bsym_table t) ps
    in
    let argtype = if n = 0 then "1" else String.concat " * " farglist in
    let flx_binding =
      match kind with 
      | `Cfun -> 
        begin match ret with
        | BTYP_void -> "  proc " ^ export_name ^ " : " ^ argtype  ^ ";"
        | _ -> "  fun " ^ export_name ^ " : " ^ argtype ^ " -> " ^ sbt bsym_table ret ^ ";"
        end
      | `Fun -> 
        let carglist = ref [] in
        let nargs = for i = 1 to n do carglist := ("$" ^ string_of_int (n-i+1)) :: (!carglist) done in
        let carglist = String.concat "," (!carglist) in
        let carglist = 
            (* HACK to cast client thread frame pointer to this library, will NOT
               work if the library has any state i.e. variables, but is enough to 
               pass over the GC, provided the call is made in a suitable context.
             *)

            "(::flxusr::"^mname^"::thread_frame_t*)(void*)ptf" ^ if n = 0 then "" else ","^carglist
        in
        let big = (String.length carglist + String.length argtype) > 40 in
        let sep = if big then "\n    " else " " in
        begin match ret with
        | BTYP_void -> 
          "  proc " ^ export_name ^ ":" ^ sep ^ 
          argtype ^ " =" ^ sep ^
          "\"" ^ export_name ^ "(" ^carglist^ ");\""^ sep ^ 
          "\" requires property \"needs_ptf\";" 

        | _ -> "  fun " ^ export_name ^ ":" ^ sep ^ 
          argtype ^ " ->" ^ sep ^ 
          sbt bsym_table ret ^ " =" ^ sep ^
          "\"" ^ export_name ^ "("^carglist^")\"" ^ sep ^
          "requires property \"needs_ptf\";" 
        end
    in
    flx_binding ^ " // "^fkind ^ " " ^ cpp_instance_name syms bsym_table index []^"\n"

  | _ -> failwith "Not implemented: export non-function/procedure"
  end

let gen_felix_struct_export syms bsym_table idx modulename =
  let bsym = Flx_bsym_table.find bsym_table idx in
  let sname = Flx_bsym.id bsym in
  let bbdcl = Flx_bsym.bbdcl bsym in
  let fields = match bbdcl with 
    | BBDCL_struct ([],fields) -> fields
    | _ -> assert false
  in
  let mkmem (id,t) = "      " ^ id ^ ": "^ sbt bsym_table t ^ ";\n" in
  let mems = catmap "" mkmem fields in
  "  cstruct " ^ sname ^ " {\n" ^
  mems ^
  "  };\n"

let gen_felix_union_export syms bsym_table idx modulename =
  let bsym = Flx_bsym_table.find bsym_table idx in
  let sname = Flx_bsym.id bsym in
  let bbdcl = Flx_bsym.bbdcl bsym in
  let fields = match bbdcl with 
    | BBDCL_union ([],fields) -> fields
    | _ -> assert false
  in
  let mkmem (id,seq,evs,t,_,_) = "     | " ^ id ^ " = " ^ string_of_int seq ^ " " ^ 
     (match t with BTYP_void -> "" | _ -> " of "^ sbt bsym_table t) ^ "\n" 
  in
  let mems = catmap "" mkmem fields in
  "  union " ^ sname ^ " =\n" ^
  mems ^
  "  ;\n"

let gen_felix_requirement_export syms bsym_table breqs modulename =
  (* output requirements reverse in dependency order *)
  let get_child_rqs idx =
    let bsym = Flx_bsym_table.find bsym_table idx in 
    let bbdcl = Flx_bsym.bbdcl bsym in
    match bbdcl with
    | BBDCL_external_code (bvs, cs, ikind,breqs) -> breqs
    | _ -> assert false
  in
  let rec scan rqs out =
    match List.rev rqs with (* to preserve order of writing *)
    | [] -> out
    | (idx,ts) :: tail -> 
      assert (ts = []);
      let out = if List.mem idx out then out else 
        let out = idx :: out in
        let child_rqs = get_child_rqs idx in
        scan child_rqs out
      in
      scan tail out
  in
  let rq_list = scan breqs [] in
  let lst = fold_left (fun acc (idx) -> 
    let bsym = Flx_bsym_table.find bsym_table idx in 
    let name = Flx_bsym.id bsym in
    let bbdcl = Flx_bsym.bbdcl bsym in
    let req = 
     match bbdcl with
    | BBDCL_external_code (bvs, cs, ikind,breqs) ->
      let open Flx_code_spec in
      let cs = match cs with
      | Str s -> s
      | Str_template s -> s
      | Virtual -> "" (* "VIRTUAL CODE SPEC?"*)
      | Identity -> "" (* "IDENTITY CODE SPEC?"*)
      in
      if cs <> "" then
        begin match ikind with
        | `Header -> "    requires header '''" ^ cs ^ "''';\n"
        | `Body -> "    requires body '''" ^ cs ^ "''';\n"
        | `Package -> "    requires package '" ^ cs ^ "';\n"
        end 
      else ""
    | _ -> assert false 
    in 
    (* "// requirement index " ^ si idx ^ "name=" ^ name ^ "\n" ^ *)
    if List.mem req acc then acc else req :: acc
  )
  []
  rq_list 
  in
  "// Export requirements\n" ^
  cat "" lst
  ^ "\n"

let gen_biface_felix1 syms bsym_table modulename biface = match biface with
  | BIFACE_export_python_fun (sr,index, export_name) ->
    "  // PYTHON FUNCTION " ^ export_name ^ "\n"

  | BIFACE_export_fun (sr,index, export_name) ->
    gen_felix_binding syms bsym_table `Fun index export_name modulename


  | BIFACE_export_cfun (sr,index, export_name) ->
    gen_felix_binding syms bsym_table `Cfun index export_name modulename

  | BIFACE_export_type (sr, typ, export_name) ->
     "  // TYPE " ^ export_name ^ "\n"

  | BIFACE_export_struct (sr,idx) ->
    gen_felix_struct_export syms bsym_table idx modulename

  | BIFACE_export_union (sr,idx, export_name) ->
    gen_felix_union_export syms bsym_table idx modulename

  | BIFACE_export_requirement (sr,breqs) ->
    gen_felix_requirement_export syms bsym_table breqs modulename


let gen_biface_headers syms bsym_table bifaces modulename =
  cat "" (List.map (gen_biface_header syms bsym_table modulename) bifaces)

let gen_biface_bodies syms bsym_table shapes shape_map label_map bifaces =
  cat "" (List.map (gen_biface_body syms bsym_table shapes shape_map label_map) bifaces)

let gen_biface_felix syms bsym_table bifaces modulename =
  let mname = Flx_name.cid_of_flxid modulename in
  "class " ^ modulename ^ "_interface {\n" ^
  "  requires header '''\n" ^
  "    #define FLX_EXTERN_" ^ mname ^ " FLX_IMPORT\n" ^
  "    #include \""^modulename^".hpp\"\n" ^
  "  ''';\n" ^
  cat "" (List.map (gen_biface_felix1 syms bsym_table modulename) bifaces) ^
  "}\n"


