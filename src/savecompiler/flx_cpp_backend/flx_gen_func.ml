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
open Flx_btype_subst

(* vs here is the (name,index) list of type variables *)
let gen_function syms bsym_table props index id sr vs bps ret' ts instance_no =
  let stackable = mem `Stack_closure props in
  let heapable = mem `Heap_closure props in
  let requires_ptf = mem `Requires_ptf props in
  let yields = mem `Yields props in
  if heapable && (not requires_ptf) then begin
    print_endline ("The function " ^ id ^ (if requires_ptf then " REQUIRES PTF" else " DOES NOT REQUIRE PTF"));
    print_endline ("The function " ^ id ^ (if heapable then "HEAPABLE" else " NOT HEAPABLE"));
    print_endline "CONFLICT! Heapable implies requires ptf!";
  end
  ;
  (*
  let strb x y = (if x then " is " else " is not " ) ^ y in
  print_endline ("The function " ^ id ^ strb stackable "stackable");
  print_endline ("The function " ^ id ^ strb heapable "heapable");
  *)
  (*
  let heapable = not stackable or heapable in
  *)
  let rt vs t = beta_reduce "flx_gen_func: gen_function" 
    syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) 
  in
(*
  print_endline ("The function " ^ id ^ (if requires_ptf then " REQUIRES PTF" else " DOES NOT REQUIRE PTF"));
*)
(*
  let ps = List.map (fun {pid=id; pindex=ix; ptyp=t} -> id,t) bps in
*)
  if syms.compiler_options.print_flag then
  print_endline
  (
    "//Generating function inst " ^
    string_of_bid instance_no ^ "=" ^
    id ^ "<" ^ string_of_bid index ^ ">" ^
    (
      if length ts = 0 then ""
      else "[" ^ catmap "," (sbt bsym_table) ts ^ "]"
    )
  );
  let argtype = Flx_bparams.get_btype bps in
  if length ts <> length vs then
  failwith
  (
    "[gen_function] wrong number of args, expected vs = " ^
    si (length vs) ^
    ", got ts=" ^
    si (length ts)
  );
  let argtype = rt vs argtype in
  let rt' vs t = beta_reduce "flx_gen_func: 2" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
  let ret = rt' vs ret' in
  if ret = btyp_tuple [] then "// elided (returns unit)\n" else

  let funtype = Flx_fold.fold bsym_table syms.counter (btyp_function (argtype, ret)) in

  let argtypename = cpp_typename syms bsym_table argtype in
  let funtypename =
    if mem `Heap_closure props then
      try Some (cpp_type_classname syms bsym_table funtype)
      with _ -> None
    else None
  in
  let display = get_display_list bsym_table index in
  let frame_dcls =
    if requires_ptf then
    "  FLX_FMEM_DECL\n"
    else ""
  in
  let pc_dcls =
    if yields then
    "  FLX_PC_DECL\n"
    else ""
  in
  let display_string = match display with
    | [] -> ""
    | display ->
      cat ""
      (
        List.map
        (fun (i, vslen) ->
         try
         let instname = cpp_instance_name syms bsym_table i (list_prefix ts vslen) in
         "  " ^ instname ^ " *ptr" ^ instname ^ ";\n"
         with _ -> failwith "Can't cal display name"
         )
        display
      )
  and ctor_dcl name =
    "  " ^name^
    (if length display = 0
    then (if requires_ptf then "(FLX_FPAR_DECL_ONLY);\n" else "();\n")
    else (
    "  (" ^
    (if requires_ptf then
    "FLX_FPAR_DECL "
    else ""
    )
    ^
    cat ", "
      (
        List.map
        (
          fun (i,vslen) ->
          let instname = cpp_instance_name syms bsym_table i (list_prefix ts vslen) in
          instname ^ "*"
        )
        display
      )^
      ");\n"
    ))
  (*
  and dtor_dcl name =
    "  ~" ^ name ^"();\n"
  *)
  in
  let members = find_members syms bsym_table index ts in
  match ret with
  | BTYP_fix (0,_)
  | BTYP_void ->
    let name = cpp_instance_name syms bsym_table index ts in
    let ctor = ctor_dcl name in
    "struct " ^ name ^
    (match funtypename with
    | Some x -> ": "^x
    | None -> if not heapable then "" else ": ::flx::rtl::con_t"
    )
    ^
    " {\n" ^
    (*
    "  //os frames\n" ^
    *)
    frame_dcls ^
    (*
    "  //display\n" ^
    *)
    display_string ^ "\n" ^
    members ^
    (*
    "  //constructor\n" ^
    *)
    ctor ^
    (
      if mem `Heap_closure props then
      (*
      "  //clone\n" ^
      *)
      "  " ^name^"* clone();\n"
      else ""
    )
    ^
    (*
    "  //call\n" ^
    *)
    (if argtype = btyp_tuple [] || argtype = btyp_void ()
    then
      (if stackable then "  void stack_call();\n" else "") ^
      (if heapable then "  ::flx::rtl::con_t *call(::flx::rtl::con_t*);\n" else "")
    else
      (if stackable then "  void stack_call("^argtypename^" const &);\n" else "") ^
      (if heapable then "  ::flx::rtl::con_t *call(::flx::rtl::con_t*,"^argtypename^" const &);\n" else "")
    ) ^
    (*
    "  //resume\n" ^
    *)
    (if heapable then "  ::flx::rtl::con_t *resume();\n" else "")
    ^
    "};\n"

  | _ ->
    let name = cpp_instance_name syms bsym_table index ts in
    let rettypename = cpp_typename syms bsym_table ret in
    let ctor = ctor_dcl name in
    "struct " ^ name ^
    (match funtypename with
    | Some x -> ": "^x
    | None -> ""
    )
    ^
    " {\n" ^
    (*
    "  //os frames\n" ^
    *)
    frame_dcls ^
    pc_dcls ^
    (*
    "  //display\n" ^
    *)
    display_string ^ "\n" ^
    members ^
    (*
    "  //constructor\n" ^
    *)
    ctor ^
    (
      if mem `Heap_closure props then
      (*
      "  //clone\n" ^
      *)
      "  " ^name^"* clone();\n"
      else ""
    )
    ^
    (*
    "  //apply\n" ^
    *)
    "  "^rettypename^
    " apply(" ^
    (if argtype = btyp_tuple [] || argtype = btyp_void () then ""
    else argtypename^" const &")^
    ");\n"  ^
    "};\n"



