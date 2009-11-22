(* code generation driver *)

open Flx_util
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_name
open Flx_tgen
open Flx_gen
open Flx_getopt
open Flx_version
open Flx_exceptions
open Flx_flxopt
open Flx_ogen
open Flx_typing
open List
;;
Flx_version_hook.set_version ();;

let print_help () = print_options(); exit(0)
;;

let reverse_return_parity = ref false
;;

let last_time = ref 0.0
;;
let tim() =
  let now = (Unix.times()).Unix.tms_utime in
  let elapsed = now -. !last_time in
  last_time := now;
  elapsed
;;

let format_time tm =
  si (tm.Unix.tm_year + 1900) ^ "/" ^
  si (tm.Unix.tm_mon + 1) ^ "/" ^
  si tm.Unix.tm_mday ^ " " ^
  si tm.Unix.tm_hour ^ ":" ^
  si tm.Unix.tm_min ^ ":" ^
  si tm.Unix.tm_sec
;;

type file_state_t = [`NeverOpened | `Open of out_channel | `Closed ]

type out_file_t = {
  out_filename : string;
  mutable out_chan: file_state_t;
}

let force_open f =
  match f.out_chan with
  | `Open chan -> chan
  | _ ->
    let chan = open_out f.out_filename in
    f.out_chan <- `Open chan;
    chan

let ensure_closed f =
  match f.out_chan with
  | `NeverOpened | `Closed -> ()
  | `Open chan ->
    close_out chan;
    f.out_chan <- `Closed

let was_used f =
  match f.out_chan with
  | `NeverOpened -> false
  | _ -> true

let ws f x =
  let chan = force_open f in
  output_string chan x

let wl f x = ws f (x ^ "\n")

let mkf f = { out_filename=f; out_chan=`NeverOpened }

;;

try
  (* Time initialisation *)
  let compile_start = Unix.time () in
  let compile_start_gm = Unix.gmtime compile_start in
  let compile_start_local = Unix.localtime compile_start in
  let compile_start_gm_string = format_time compile_start_gm ^ " UTC" in
  let compile_start_local_string = format_time compile_start_local ^ " (local)" in


  (* Argument parsing *)
  let argc = Array.length Sys.argv in
  if argc <= 1
  then begin
    print_endline "usage: flxg --key=value ... filename; -h for help";
    exit 0
  end
  ;
  let raw_options = parse_options Sys.argv in
  let compiler_options = get_felix_options raw_options in
  reverse_return_parity := compiler_options.reverse_return_parity
  ;
  let syms = make_syms compiler_options in
  if check_keys raw_options ["h"; "help"]
  then print_help ()
  ;
  if check_key raw_options "version"
  then (print_endline ("Felix Version " ^ !version_data.version_string))
  ;
  let print_debug msg =
    if compiler_options.print_flag
    then print_endline msg
  in

  if compiler_options.print_flag then begin
    print_string "//Include directories = ";
    List.iter (fun d -> print_string (d ^ " "))
    compiler_options.include_dirs;
    print_endline ""
  end
  ;

  (* main filename processing *)
  (*
  List.iter (fun f ->
  print_endline ("File " ^ f))
  compiler_options.files
  ;
  *)
  let files = compiler_options.files in
  if length files = 0 then print_help();

  let filename = hd (rev files) in
  let inbase = filename in

  let input_file_name = inbase ^ ".flx" in
  (*
  and iface_file_name = filebase ^ ".fix"
  *)
  let outbase =
    match compiler_options.output_dir with
    | None -> filename
    | Some d -> Filename.concat d (Filename.basename filename)
  in
  let why_file_name = mkf (outbase ^ ".why")

  and header_file = mkf (outbase ^ ".hpp")
  and body_file = mkf (outbase ^ ".cpp")
  and ctors_file = mkf (outbase ^ "_ctors.cpp")
  and package_file = mkf (outbase ^ ".resh")
  and rtti_file = mkf (outbase ^ ".rtti")
  and report_file = mkf (outbase ^ ".xref")

  and module_name =
    let n = String.length inbase in
    let i = ref (n-1) in
    while !i <> -1 && inbase.[!i] <> '/' && inbase.[!i] <> '\\' do decr i done;
    String.sub inbase (!i+1) (n - !i - 1)
  in

  let include_dirs =  Filename.current_dir_name :: compiler_options.include_dirs in
  let compiler_options = { compiler_options with include_dirs = include_dirs } in
  let syms = { syms with compiler_options = compiler_options } in

  (* PARSE THE IMPLEMENTATION FILE *)

  let parse_tree =
    fold_left (fun tree file ->
      let file_name =
        if Filename.check_suffix file ".flx" then file else file ^ ".flx"
      in
      print_debug ("//Parsing Implementation " ^ file_name);
      let sts = Flx_colns.include_file syms file_name in
      concat [tree; sts]
    )
    []
    files
  in
  print_debug (Flx_print.string_of_compilation_unit parse_tree);

  let parse_time = tim() in
  print_debug ("//PARSE OK time " ^ string_of_float parse_time);

  print_debug "//DESUGARING";

  let desugar_state = Flx_desugar.make_desugar_state module_name syms in
  let asms = Flx_desugar.desugar_compilation_unit desugar_state parse_tree in
  let desugar_time = tim() in
  print_debug ("//DESUGAR time " ^ string_of_float desugar_time);

  (* THIS IS A HACK! *)
  let root = !(syms.counter) in
  print_debug ("//Top level module '" ^ module_name ^ "' has index " ^
    string_of_bid root);

  (* Bind the assemblies. *)
  let bind_state = Flx_bind.make_bind_state syms in
  let bsym_table = Flx_bind.bind_asms bind_state asms in

  let child_map = Flx_child.cal_children bsym_table in
  Flx_typeclass.typeclass_instance_check syms bsym_table child_map;

  (* generate axiom checks *)
  if compiler_options.generate_axiom_checks then
  Flx_axiom.axiom_check syms bsym_table;

  (* generate why file *)
  Flx_why.emit_whycode why_file_name.out_filename syms bsym_table root;

  let binding_time = tim() in

  print_debug "//CHECKING ROOT";
  let root_proc =
    match
      try Flx_sym_table.find syms.sym_table root
      with Not_found ->
        failwith
        (
          "Can't find root module " ^ string_of_bid root ^
          " in symbol table?"
        )
    with { Flx_sym.id=id; pubmap=name_map;symdef=entry} ->
    begin match entry with
      | SYMDEF_module -> ()
      | _ -> failwith "Expected to find top level module ''"
    end
    ;
    let entry =
      try Hashtbl.find name_map "_init_"
      with Not_found ->
        failwith "Can't find name _init_ in top level module's name map"
    in
    let index = match entry with
      | FunctionEntry [x] -> sye x
      | FunctionEntry [] -> failwith "Couldn't find '_init_'"
      | FunctionEntry _ -> failwith "Too many top level procedures called '_init_'"
      | NonFunctionEntry _ -> failwith "_init_ found but not procedure"
    in
    print_debug ("//root module's init procedure has index " ^
      string_of_bid index);
    index
  in

  (* Optimize the bound values *)
  let bsym_table, _ = Flx_opt.optimize_bsym_table
    syms
    bsym_table
    root_proc
  in

  let opt_time = tim() in
  print_debug ("//Optimisation complete time " ^ string_of_float opt_time);

  (* Lower the bound symbols for the backend. *)
  let bsym_table, child_map = Flx_lower.lower_bsym_table
    (Flx_lower.make_lower_state syms)
    bsym_table
    root_proc
  in

  (* Start working on the backend. *)

  let label_map = Flx_label.create_label_map bsym_table syms.counter in
  let label_usage = Flx_label.create_label_usage syms bsym_table label_map in
  let label_info = label_map, label_usage in

  (* Make sure we can find the _init_ instance *)
  let top_class =
    try cpp_instance_name syms bsym_table root_proc [] with Not_found ->
      failwith ("can't name instance of root _init_ procedure index " ^
        string_of_bid root_proc)
  in

  (* FUDGE the init procedure to make interfacing a bit simpler *)
  let topclass_props =
    let id,parent,sr,entry = Flx_bsym_table.find bsym_table root_proc in
    match entry with
    | BBDCL_procedure (props,vs,p,exes) -> props
    | _ -> syserr sr "Expected root to be procedure"
  in
  print_debug ("//root module's init procedure has name " ^ top_class);

  let instantiation_time = tim() in

  print_debug ("//instantiation time " ^ string_of_float instantiation_time);

  if compiler_options.compile_only
  then exit (if compiler_options.reverse_return_parity then 1 else 0)
  ;

  let psh s = ws header_file s in
  let psb s = ws body_file s in
  let psp s = ws package_file s in
  let psr s = ws rtti_file s in

  let plh s = psh s; psh "\n" in
  let plb s = psb s; psb "\n" in
  let plr s = psr s; psr "\n" in
  let plp s = psp s; psp "\n" in

  print_debug "//GENERATING Package Requirements";

  (* These must be in order: build a list and sort it *)
  begin
    let dfnlist = ref [] in
    Hashtbl.iter
    (fun (i,ts) _ -> dfnlist := (i,ts) :: !dfnlist)
    syms.instances
    ;
    let insts = Hashtbl.create 97 in
    List.iter
    (fun (i,ts)->
      match
        try Flx_bsym_table.find bsym_table i with Not_found ->
          failwith ("[package] can't find index " ^ string_of_bid i)
      with (id,parent,sr,entry) ->
      match entry with
      | BBDCL_insert (_,s,`Package,_) ->
        begin match s with
        | CS_identity | CS_str "" | CS_str_template "" -> ()
        | _ ->
          let s =
            match s with
            | CS_identity -> assert false (* covered above *)
            | CS_virtual -> clierr sr "Instantiate virtual insertion!"
            | CS_str s -> Flx_cexpr.ce_expr "atom" s
            | CS_str_template s ->
              (* do we need tsubst vs ts t? *)
              let tn t = cpp_typename syms t in
              let ts = List.map tn ts in
              Flx_csubst.csubst sr sr s (Flx_cexpr.ce_atom "Error") [] [] "Error" "Error" ts "atom" "Error" ["Error"] ["Error"] ["Error"]
          in
          let s = Flx_cexpr.sc "expr" s in
          if not (Hashtbl.mem insts s) then
          begin
            Hashtbl.add insts s ();
            plp s
          end
        end
      | _ -> ()
    )
    (List.sort compare !dfnlist)
  end
  ;


  print_debug "//GENERATING C++: user headers";

  plh ("#ifndef _FLX_GUARD_" ^ cid_of_flxid module_name);
  plh ("#define _FLX_GUARD_" ^ cid_of_flxid module_name);
  plh ("//Input file: " ^ input_file_name);
  plh ("//Generated by Felix Version " ^ !version_data.version_string);
  plh ("//Timestamp: " ^ compile_start_gm_string);
  plh ("//Timestamp: " ^ compile_start_local_string);
  plh "";
  plh "#ifndef FLX_NO_INCLUDES";
  plh ("#include \"" ^ module_name ^ ".includes\"");
  plh "#endif";
  plh "//FELIX RUNTIME";
  (* plh "#include \"flx_rtl.hpp\""; *)
  plh "using namespace flx::rtl;";
  (* plh "#include \"flx_gc.hpp\""; *)
  plh "using namespace flx::gc::generic;";
  plh "";

  plh "\n//-----------------------------------------";
  plh "//USER HEADERS";
  (* These must be in order: build a list and sort it *)
  begin
    let dfnlist = ref [] in
    Hashtbl.iter
    (fun (i,ts) _ -> dfnlist := (i,ts) :: !dfnlist)
    syms.instances
    ;
    let insts = Hashtbl.create 97 in
    List.iter
    (fun (i,ts)->
      match
        try Flx_bsym_table.find bsym_table i with Not_found ->
          failwith ("[user header] can't find index " ^ string_of_bid i)
      with (id,parent,sr,entry) ->
      match entry with
      | BBDCL_insert (_,s,`Header,_) ->
        begin match s with
        | CS_identity | CS_str "" | CS_str_template "" -> ()
        | _ ->
          let s =
            match s with
            | CS_identity -> assert false
            | CS_virtual -> clierr sr "Instantiate virtual insertion!"
            | CS_str s -> Flx_cexpr.ce_expr "atom" s
            | CS_str_template s ->
              (* do we need tsubst vs ts t? *)
              let tn t = cpp_typename syms t in
              let ts = List.map tn ts in
              Flx_csubst.csubst sr sr s (Flx_cexpr.ce_atom "Error") [] [] "Error" "Error" ts "atom" "Error" ["Error"] ["Error"] ["Error"]
          in
          let s = Flx_cexpr.sc "expr" s in
          if not (Hashtbl.mem insts s) then
          begin
            Hashtbl.add insts s ();
            plh s
          end
        end
      | _ -> ()
    )
    (List.sort compare !dfnlist)
  end
  ;

  plh "\n//-----------------------------------------";
  List.iter plh [
  "//FELIX SYSTEM";
  "namespace flxusr { namespace " ^ cid_of_flxid module_name ^ " {";
  "struct thread_frame_t;"
  ]
  ;
  print_debug "//GENERATING C++: collect types";
  let types = ref [] in
  Hashtbl.iter (fun t index -> types := (index, t) :: !types) syms.registry;

  let types = List.sort (fun a1 a2 -> compare (fst a1) (fst a2)) !types in
  (*
  List.iter
  (fun (_,t) -> print_endline (string_of_btypecode sym_table t))
  types
  ;
  *)

  print_debug "//GENERATING C++: type class names";
  plh "\n//-----------------------------------------";
  plh "//NAME THE TYPES";
  plh  (gen_type_names syms bsym_table types);

  print_debug "//GENERATING C++: type class definitions";
  plh "\n//-----------------------------------------";
  plh  "//DEFINE THE TYPES";
  plh  (gen_types syms bsym_table types);

  print_debug "//GENERATING C++: function and procedure classes";
  plh "\n//-----------------------------------------";
  plh  "//DEFINE FUNCTION CLASS NAMES";
  plh  (gen_function_names syms bsym_table child_map);

  plh "\n//-----------------------------------------";
  plh  "//DEFINE FUNCTION CLASSES";
  plh  (gen_functions syms bsym_table child_map);

  let topvars_with_type = find_thread_vars_with_type bsym_table in
  let topvars = List.map fst topvars_with_type in
  List.iter plh
  [
  "struct thread_frame_t {";
  "  int argc;";
  "  char **argv;";
  "  FILE *flx_stdin;";
  "  FILE *flx_stdout;";
  "  FILE *flx_stderr;";
  "  gc_profile_t *gcp;";
  "  thread_frame_t(";
  "  );";
  ]
  ;
  plh (format_vars syms bsym_table topvars []);
  plh "};";
  plh "";
  plh "FLX_DCL_THREAD_FRAME";
  plh "";
  plh ("}} // namespace flxusr::" ^ cid_of_flxid module_name);

  (* BODY *)
  print_debug "//GENERATING C++: GC ptr maps & offsets";

  plb ("//Input file: " ^ input_file_name);
  plb ("//Generated by Felix Version " ^ !version_data.version_string);
  plb ("//Timestamp: " ^ compile_start_gm_string);
  plb ("//Timestamp: " ^ compile_start_local_string);

  plb ("#include \"" ^ module_name ^ ".hpp\"");
  plb "#include <stdio.h>"; (* for diagnostics *)

  plb "#define comma ,";
  plb "\n//-----------------------------------------";
  plb "//EMIT USER BODY CODE";
  (* These must be in order: build a list and sort it *)
  begin
    let dfnlist = ref [] in
    Hashtbl.iter
    (fun (i,ts) _ -> dfnlist := (i,ts) :: !dfnlist)
    syms.instances
    ;
    let insts = Hashtbl.create 97 in
    List.iter
    (fun (i,ts) ->
      match
        try Flx_bsym_table.find bsym_table i with Not_found ->
          failwith ("[user body] can't find index " ^ string_of_bid i)
      with (id,parent,sr,entry) ->
      match entry with
      | BBDCL_insert (_,s,`Body,_) ->
        begin match s with
        | CS_identity | CS_str "" | CS_str_template "" -> ()
        | _ ->
          let s =
            match s with
            | CS_identity -> assert false
            | CS_virtual -> clierr sr "Instantiate virtual insertion!"
            | CS_str s -> Flx_cexpr.ce_expr "atom" s
            | CS_str_template s ->
              (* do we need tsubst vs ts t? *)
              let tn t = cpp_typename syms t in
              let ts = List.map tn ts in
              Flx_csubst.csubst sr sr s (Flx_cexpr.ce_atom "Error") [] [] "Error" "Error" ts "atom" "Error" ["Error"] ["Error"] ["Error"]
          in
          let s = Flx_cexpr.sc "expr" s in
          if not (Hashtbl.mem insts s) then
          begin
            Hashtbl.add insts s ();
            plb s
          end
        end
      | _ -> ()
    )
    (List.sort compare !dfnlist)
  end
  ;

  plb "\n//-----------------------------------------";
  plb ("namespace flxusr { namespace " ^ cid_of_flxid module_name ^ " {");

  plb "FLX_DEF_THREAD_FRAME";
  plb "//Thread Frame Constructor";

  let sr = Flx_srcref.make_dummy "Thread Frame" in
  let topfuns = List.filter (fun (_,t) -> is_gc_pointer syms bsym_table sr t) topvars_with_type in
  let topfuns = List.map fst topfuns in
  let topinits =
    [
      "  gcp(0)"
    ]
    @
    List.map
    (fun index ->
      "  " ^
      cpp_instance_name syms bsym_table index [] ^
      "(0)"
    )
    topfuns
  in
  let topinits = String.concat ",\n" topinits in
  List.iter plb
  [
  "thread_frame_t::thread_frame_t(";
  ") :";
  topinits;
  "{}"
  ];

  plr (Flx_ogen.gen_offset_tables syms bsym_table child_map module_name);
  ensure_closed rtti_file;
  if was_used rtti_file then begin
    plb "\n//-----------------------------------------";
    plb "//DEFINE OFFSET tables for GC";
    plb ("#include \""^module_name^".rtti\"");
  end;

  begin
    let header_emitted = ref false in
    Hashtbl.iter
    (fun (fno,_) inst ->
      try
        let labels = Hashtbl.find label_map fno in
        Hashtbl.iter
        (fun lab lno ->
          match Flx_label.get_label_kind_from_index label_usage lno with
          | `Far ->
            if not !header_emitted then begin
              plb "\n//-----------------------------------------";
              plb "#if FLX_CGOTO";
              plb "//DEFINE LABELS for GNUC ASSEMBLER LABEL HACK";
              header_emitted := true;
            end
            ;
            plb ("FLX_DECLARE_LABEL(" ^ string_of_bid lno ^ "," ^
              string_of_bid inst ^ "," ^ lab ^ ")")
          | `Near -> ()
          | `Unused -> ()
        )
        labels
      with Not_found -> ()
    )
    syms.instances
    ;
    if !header_emitted then plb "#endif";
  end
  ;

  print_debug "//GENERATING C++: method bodies";

  plb "\n//-----------------------------------------";
  plb "//DEFINE FUNCTION CLASS METHODS";
  plb ("#include \"" ^module_name ^ "_ctors.cpp\"");
  gen_execute_methods
    body_file.out_filename
    syms
    bsym_table
    child_map
    label_info
    syms.counter
    (force_open body_file)
    (force_open ctors_file);

  print_debug "//GENERATING C++: interface";
  plb "\n//-----------------------------------------";
  plb ("}} // namespace flxusr::" ^ cid_of_flxid module_name);

  plb "//CREATE STANDARD EXTERNAL INTERFACE";
  plb ("FLX_FRAME_WRAPPERS(flxusr::" ^ cid_of_flxid module_name ^ ")");
  (if List.mem `Pure topclass_props then
    plb ("FLX_C_START_WRAPPER(flxusr::" ^ cid_of_flxid module_name ^ "," ^ top_class ^ ")")
  else if List.mem `Stackable topclass_props then
    plb ("FLX_STACK_START_WRAPPER(flxusr::" ^ cid_of_flxid module_name ^ "," ^ top_class ^ ")")
  else
    plb ("FLX_START_WRAPPER(flxusr::" ^ cid_of_flxid module_name ^ "," ^ top_class ^ ")")
  );
  plb "\n//-----------------------------------------";

  plh ("using namespace flxusr::" ^ cid_of_flxid module_name ^ ";");
  if List.length syms.bifaces > 0 then begin
    plh "//DECLARE USER EXPORTS";
    plh (gen_biface_headers syms bsym_table syms.bifaces);
    plb "//DEFINE EXPORTS";
    plb (gen_biface_bodies syms bsym_table syms.bifaces);
    plb (gen_python_module module_name syms bsym_table syms.bifaces);
  end
  ;

  (* rather late: generate variant remapping tables *)
  if Hashtbl.length syms.variant_map > 0 then begin
    plr "// VARIANT REMAP ARRAYS";
    Hashtbl.iter
    (fun (srct,dstt) vidx ->
      match srct,dstt with
      | BTYP_variant srcls, BTYP_variant dstls ->
        begin
          let rcmp (s,_) (s',_) = compare s s' in
          let srcls = List.sort rcmp srcls in
          let dstls = List.sort rcmp dstls in
          let n = List.length srcls in
          let remap =
            List.map
            (fun (s,_) ->
              match Flx_list.list_assoc_index dstls s with
              | Some i -> i
              | None -> assert false
            )
            srcls
          in
          plr ("static int vmap_" ^ string_of_bid vidx ^ "[" ^ si n ^ "]={" ^
            catmap "," (fun i -> si i) remap ^
          "};")
        end
      | _ -> failwith "Remap non variant types??"
    )
    syms.variant_map
  end
  ;
  plh "//header complete";
  plh "#endif";
  plb "//body complete";
  ensure_closed header_file;
  ensure_closed body_file;
  ensure_closed ctors_file;
  plp "flx";
  plp "flx_gc";  (* RF: flx apps now need flx_gc. is this the way to do it? *)
  ensure_closed package_file;
  let code_generation_time = tim() in
  print_debug ("//code generation time " ^ string_of_float code_generation_time);

  let total_time =
    parse_time +.
    desugar_time +.
    binding_time +.
    opt_time +.
    instantiation_time +.
    code_generation_time
  in
  print_debug ("//Felix compiler time " ^ string_of_float total_time);
  let fname = "flxg_stats.txt" in
  let
    old_parse_time,
    old_desugar_time,
    old_binding_time,
    old_opt_time,
    old_instantiation_time,
    old_code_generation_time,
    old_total_time
  =
  let zeroes = 0.0,0.0,0.0,0.0,0.0,0.0,0.0 in
  let f = try Some (open_in fname) with _ -> None in
  begin match f with
  | None -> zeroes
  | Some f ->
    let x =
      try
        let id x1 x2 x3 x4 x5 x6 x7 = x1, x2, x3, x4, x5, x6, x7 in
        Scanf.fscanf f
        "parse=%f desugar=%f bind=%f opt=%f inst=%f gen=%f tot=%f"
        id
      with _ -> zeroes
    in close_in f; x
  end
  in
    try (* failure to save stats isn't fatal *)
      let f = open_out fname in
      Printf.fprintf
        f
        "parse=%f\ndesugar=%f\nbind=%f\nopt=%f\ninst=%f\ngen=%f\ntot=%f\n"
        (old_parse_time +. parse_time)
        (old_desugar_time +. desugar_time)
        (old_binding_time +. binding_time)
        (old_opt_time +. opt_time)
        (old_instantiation_time +. instantiation_time)
        (old_code_generation_time +. code_generation_time)
        (old_total_time +. total_time)
      ;
      close_out f
    with _ -> ()
    ;
  exit (if compiler_options.reverse_return_parity then 1 else 0)

with x ->
  Flx_terminate.terminate !reverse_return_parity x
;;
