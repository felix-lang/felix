(* code generation driver *)

open Format

open Flx_util
open Flx_types
open Flx_btype
open Flx_bbdcl
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

module CS = Flx_code_spec
;;

(* We have to set the felix version first. *)
Flx_version_hook.set_version ()


type file_state_t = [`NeverOpened | `Open of out_channel | `Closed ]

type out_file_t = {
  out_filename : string;
  mutable out_chan: file_state_t;
}


let mkf f = { out_filename=f; out_chan=`NeverOpened }


(* Parse the felix arguments and do some option parsing while we're at it. *)
let parse_args () =
  (* Argument parsing *)
  let argc = Array.length Sys.argv in

  (* Error out if we don't have enough arguments. *)
  if argc <= 1 then begin
    print_endline "usage: flxg --key=value ... filename; -h for help";
    exit 1
  end;

  (* Now, parse those arguments *)
  let raw_options = parse_options Sys.argv in

  (* Print help and version out. *)
  if check_keys raw_options ["h"; "help"] then begin
    print_options ();
    exit 0
  end;

  if check_key raw_options "version" then begin
    Printf.printf "Felix version %s\n" !version_data.version_string;
    exit 0
  end;

  (* Now extract the driver options. *)
  let compiler_options = get_felix_options raw_options in

  (* Error out if we didn't specify any files. *)
  if compiler_options.files = [] then begin
    print_options ();
    exit 1
  end;

  (* Create a formatter for logging if debugging's enabled. Otherwise, create a
   * null formatter. *)
  let ppf =
    if compiler_options.print_flag
    then err_formatter
    else make_formatter (fun _ _ _ -> ()) (fun () -> ())
  in

  fprintf ppf "// Include directories = %s\n"
    (String.concat " " compiler_options.include_dirs);

  (* Make sure the current directory is in the search path. *)
  let include_dirs =
    Filename.current_dir_name :: compiler_options.include_dirs
  in
  let compiler_options = { compiler_options with
    include_dirs = include_dirs }
  in

  ppf, compiler_options


(** The state needed for flxg compilation. *)
type flxg_state_t = {
  ppf: formatter;
  compile_start_gm_string: string;
  compile_start_local_string: string;
  syms: Flx_mtypes2.sym_state_t;
  module_name: string;
  input_filename: string;
  header_file: out_file_t;
  body_file: out_file_t;
  ctors_file: out_file_t;
  package_file: out_file_t;
  rtti_file: out_file_t;
  report_file: out_file_t;
  why_file: out_file_t;
  dep_file_name: string;
  mutable parse_time: float;
  mutable desugar_time: float;
  mutable bind_time: float;
  mutable opt_time: float;
  mutable lower_time: float;
  mutable instantiation_time: float;
  mutable codegen_time: float;
}


let make_module_name inbase =
  let n = String.length inbase in
  let i = ref (n-1) in
  while !i <> -1 && inbase.[!i] <> '/' && inbase.[!i] <> '\\' do decr i done;
  String.sub inbase (!i+1) (n - !i - 1)


(** Make the state needed for flxg compilation. *)
let make_flxg_state ppf compiler_options =
  let format_time tm =
    si (tm.Unix.tm_year + 1900) ^ "/" ^
    si (tm.Unix.tm_mon + 1) ^ "/" ^
    si tm.Unix.tm_mday ^ " " ^
    si tm.Unix.tm_hour ^ ":" ^
    si tm.Unix.tm_min ^ ":" ^
    si tm.Unix.tm_sec
  in

  (* Time initialisation *)
  let compile_start = Unix.time () in
  let compile_start_gm = Unix.gmtime compile_start in
  let compile_start_local = Unix.localtime compile_start in
  let compile_start_gm_string = format_time compile_start_gm ^ " UTC" in
  let compile_start_local_string = format_time compile_start_local ^ " (local)" in

  let filename = List.hd (List.rev compiler_options.files) in
  let inbase = filename in

  let input_filename = inbase ^ ".flx" in
  (*
  and iface_file_name = filebase ^ ".fix"
  *)
  let outbase =
    match compiler_options.output_dir with
    | None -> filename
    | Some d -> Filename.concat d (Filename.basename filename)
  in
  {
    ppf = ppf;
    compile_start_gm_string = compile_start_gm_string;
    compile_start_local_string = compile_start_local_string;
    syms = make_syms compiler_options;
    module_name = make_module_name inbase;
    input_filename = input_filename;
    header_file = mkf (outbase ^ ".hpp");
    body_file = mkf (outbase ^ ".cpp");
    ctors_file = mkf (outbase ^ ".ctors_cpp");
    package_file = mkf (outbase ^ ".resh");
    rtti_file = mkf (outbase ^ ".rtti");
    report_file = mkf (outbase ^ ".xref");
    why_file = mkf (outbase ^ ".why");
    dep_file_name = outbase ^ ".dep";
    parse_time = 0.0;
    desugar_time = 0.0;
    bind_time = 0.0;
    opt_time = 0.0;
    lower_time = 0.0;
    instantiation_time = 0.0;
    codegen_time = 0.0;
  }


let make_timer () =
  let last_time = ref 0.0 in
  fun () ->
    let now = (Unix.times()).Unix.tms_utime in
    let elapsed = now -. !last_time in
    last_time := now;
    elapsed


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


(** Parse an implementation file *)
let parse_syntax state=
  let parse_timer = make_timer () in

  let include_dirs = state.syms.compiler_options.include_dirs in
  let synfiles = List.concat (List.map (Flx_colns.render include_dirs) state.syms.compiler_options.syntax) in
  (* print_endline ("//Parsing syntax " ^ String.concat ", " synfiles); *)
  let parser_state = List.fold_left
    (fun state file -> Flx_parse.parse_syntax_file ~include_dirs state file)
    (Flx_parse.make_parser_state ())
    (synfiles)
  in

  let auto_imports = List.concat (List.map (Flx_colns.render include_dirs) state.syms.compiler_options.auto_imports) in
  let parser_state = List.fold_left
    (fun state file -> Flx_parse.parse_file ~include_dirs state file)
    parser_state 
    auto_imports
  in
  let parsing_device = !(Flx_parse_helper.global_data.Flx_token.parsing_device) in
  let parse_time = parse_timer () in
  state.parse_time <- state.parse_time +. parse_time;
  print_endline ("PARSED SYNTAX/IMPORT FILES " ^ string_of_float parse_time ^ " secs");
  let oc = open_out_bin "automaton.syntax" in
  Marshal.to_channel oc parser_state [];
  Marshal.to_channel oc parsing_device [];
  close_out oc;
  print_endline "Saved automaton to disk";
  parser_state

let load_syntax state =
  try 
     let filename = "automaton.syntax" in
     let oc = open_in_bin filename in
     let local_data = Marshal.from_channel oc in
     let parsing_device = Marshal.from_channel oc in
     close_in oc;
     (* print_endline "Loaded automaton from disk"; *)
     let env = Flx_parse_helper.global_data.Flx_token.env in
     let scm = local_data.Flx_token.scm in
     Flx_parse.load_scheme_defs env scm; 
     Flx_parse_helper.global_data.Flx_token.parsing_device := parsing_device;
     local_data
  with _ ->
    print_endline "Can't load automaton from disk: building!";
    parse_syntax state 

(** Parse an implementation file *)
let parse_file state parser_state file =
  let parse_timer = make_timer () in

  let file_name =
    if Filename.check_suffix file ".flx" then file else file ^ ".flx"
  in
  let local_prefix = Filename.chop_suffix (Filename.basename file_name) ".flx" in
  let include_dirs = state.syms.compiler_options.include_dirs in
  fprintf state.ppf "//Parsing Implementation %s\n" file_name;
  if state.syms.compiler_options.print_flag then print_endline ("Parsing " ^ file_name);
  let parser_state = Flx_parse.parse_file ~include_dirs parser_state file_name in
  let stmts = List.rev (Flx_parse.parser_data parser_state) in
  let macro_state = Flx_macro.make_macro_state local_prefix in
  let stmts = Flx_macro.expand_macros macro_state stmts in

  state.parse_time <- state.parse_time +. parse_timer ();

  stmts


(** Desugar the statements *)
let make_module module_name asms =
  let asms =
    [Dcl (
      Flx_srcref.dummy_sr,
      module_name,
      None,
      `Public,
      Flx_ast.dfltvs,
      DCL_root asms)]
  in
  asms


(** Bind the assemblies *)
let bind_asms state sym_table bsym_table start_counter asms =
  fprintf state.ppf "//BINDING\n";
  let bind_timer = make_timer () in

  let bind_state = Flx_bind.make_bind_state state.syms sym_table in
(*
print_endline "Binding asms..";
*)
  Flx_bind.bind_asms bind_state bsym_table start_counter asms;
(*
print_endline "Binding asms done";
*)
  state.bind_time <- state.bind_time +. bind_timer ();
  fprintf state.ppf "//BINDING OK time %f\n" state.bind_time


(** Generate the why file. *)
let generate_why_file state bsym_table root_proc =
  (* generate why file *)
  Flx_why.emit_whycode
    state.why_file.out_filename
    state.syms
    bsym_table
    root_proc

let generate_dep_file state =
(* NOTE: as is this can't work, because it lists the *.flx filename without the
 * flx, but it doesn't say where the *.par files are.. we need to list both,
 * since the *.par files might be in a --cache_dir directory.
 *
 * Still there's another way to use the information here: 
 * We just check the time stamps relative to the main program *.par file
 * and/or generated program, whatever flx does now with the main program
 * filename. I.e. we just take the time stamp of the main program as the 
 * largest of all the time stamps. If a file is deleted its stamp is 0,
 * which will only cause a problem if there's a dangling reference,
 * otherwise the including file had to be changed to stop this, and its
 * time stamp will be bigger.
 *)
  let chan = open_out state.dep_file_name in
  output_string chan (String.concat "\n" (!(state.syms.include_files)) ^ "\n");
  close_out chan

(** Optimize the bound symbols. *)
let optimize_bsyms state bsym_table root_proc =
  fprintf state.ppf "//OPTIMIZING\n";
  let opt_timer = make_timer () in

  let bsym_table = Flx_opt.optimize_bsym_table
    state.syms
    bsym_table
    root_proc
  in

  state.opt_time <- state.opt_time +. opt_timer ();
  fprintf state.ppf "//OPTIMIZATION OK time %f\n" state.opt_time;

  bsym_table


(** Lower the high level constructs into simpler ones. *)
let lower_bsyms state bsym_table root_proc =
  fprintf state.ppf "//LOWERING\n";
  let lower_timer = make_timer () in

  let bsym_table = Flx_lower.lower_bsym_table
    (Flx_lower.make_lower_state state.syms)
    bsym_table
    root_proc
  in

  state.lower_time <- state.lower_time +. lower_timer ();
  fprintf state.ppf "//LOWERING OK time %f\n"state.lower_time;

  bsym_table


(** Generate the C++ code. *)
let codegen_bsyms state bsym_table root_proc =
  fprintf state.ppf "//INSTANTIATING\n";
  let instantiation_timer = make_timer () in

  let label_map = Flx_label.create_label_map bsym_table state.syms.counter in
  let label_usage = Flx_label.create_label_usage state.syms bsym_table label_map in
  let label_info = label_map, label_usage in

  (* Make sure we can find the _init_ instance *)
  let top_class =
    try cpp_instance_name state.syms bsym_table root_proc [] with Not_found ->
      failwith ("can't name instance of root _init_ procedure index " ^
        string_of_bid root_proc)
  in

  (* FUDGE the init procedure to make interfacing a bit simpler *)
  let topclass_props =
    let bsym = Flx_bsym_table.find bsym_table root_proc in
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,p,BTYP_void,exes) -> props
    | _ -> syserr (Flx_bsym.sr bsym) "Expected root to be procedure"
  in
  fprintf state.ppf "//root module's init procedure has name %s\n" top_class;

  state.instantiation_time <- state.instantiation_time +. instantiation_timer ();
  fprintf state.ppf "//instantiation time %f\n" state.instantiation_time;

  (* Exit early if we're done. *)
  if state.syms.compiler_options.compile_only then () else

  (* Finally, lets do some code generation! *)

  let codegen_timer = make_timer () in

  let psh s = ws state.header_file s in
  let psb s = ws state.body_file s in
  let psp s = ws state.package_file s in
  let psr s = ws state.rtti_file s in

  let plh s = psh s; psh "\n" in
  let plb s = psb s; psb "\n" in
  let plr s = psr s; psr "\n" in
  let plp s = psp s; psp "\n" in

  fprintf state.ppf "//GENERATING Package Requirements\n";

  let instantiate_instance f insts kind (bid, ts) =
    let bsym =
      try Flx_bsym_table.find bsym_table bid with Not_found ->
        failwith ("can't find index " ^ string_of_bid bid)
    in

    match Flx_bsym.bbdcl bsym with
    | BBDCL_external_code (_,s,kind',_) when kind' = kind ->
        begin match s with
        | CS.Identity
        | CS.Str ""
        | CS.Str_template "" -> ()
        | CS.Virtual ->
            clierr (Flx_bsym.sr bsym) "Instantiate virtual insertion!"
        | _ ->
            let s =
              match s with
              | CS.Identity | CS.Virtual ->
                  assert false (* covered above *)
              | CS.Str s ->
                  Flx_cexpr.ce_expr "atom" s
              | CS.Str_template s ->
                  (* do we need tsubst vs ts t? *)
                  let tn t = cpp_typename state.syms bsym_table t in
                  let ts = List.map tn ts in
                  Flx_csubst.csubst
                    (Flx_bsym.sr bsym)
                    (Flx_bsym.sr bsym)
                    s
                    (Flx_cexpr.ce_atom "Error")
                    []
                    []
                    "Error"
                    "Error"
                    ts
                    "atom"
                    "Error"
                    ["Error"]
                    ["Error"]
                    ["Error"]
            in
            let s = Flx_cexpr.sc "expr" s in
            if not (Hashtbl.mem insts s) then begin
              Hashtbl.add insts s ();
              f s
            end
        end
      | _ -> ()
  in

  let instantiate_instances f kind =
    let dfnlist = ref [] in
    Hashtbl.iter
      (fun (i,ts) _ -> dfnlist := (i,ts) :: !dfnlist)
      state.syms.instances;

    let insts = Hashtbl.create 97 in

    List.iter
      (instantiate_instance f insts kind)
      (List.sort compare !dfnlist)
  in

  (* These must be in order: build a list and sort it *)
  instantiate_instances plp `Package;

  fprintf state.ppf "//GENERATING C++: user headers\n";

  plh ("#ifndef _FLX_GUARD_" ^ cid_of_flxid state.module_name);
  plh ("#define _FLX_GUARD_" ^ cid_of_flxid state.module_name);
  plh ("//Input file: " ^ state.input_filename);
  plh ("//Generated by Felix Version " ^ !version_data.version_string);
  plh ("//Timestamp: " ^ state.compile_start_gm_string);
  plh ("//Timestamp: " ^ state.compile_start_local_string);
  plh "";

  (* THE PRESENCE OF THESE LINES IS A BUG .. defeats the FLX_NO_INCLUDES switch
   * but we need this temporarily, at least until we fix flx.flx compilation
   * in the build system to generate dependencies in the flx.includes file
   *
   * Also we need to fix the compiler and all libraries to use explicit
   * qualification everywhere which will make the code a bit of a mess..
   *)
  plh "//FELIX RUNTIME";
  plh "#include \"flx_rtl.hpp\"";  
  (* plh "using namespace ::flx::rtl;";  *)
  plh "#include \"flx_gc.hpp\"";
  (* plh "using namespace ::flx::gc::generic;"; *)

  plh "#ifndef FLX_NO_INCLUDES";
  plh ("#include \"" ^ state.module_name ^ ".includes\"");
  plh "#endif";
  plh "";

  plh "\n//-----------------------------------------";
  plh "//USER HEADERS";

  (* These must be in order: build a list and sort it *)
  instantiate_instances plh `Header;

  plh "\n//-----------------------------------------";
  List.iter plh [
  "//FELIX SYSTEM";
  "namespace flxusr { namespace " ^ cid_of_flxid state.module_name ^ " {";
  "struct thread_frame_t;"
  ]
  ;
  fprintf state.ppf "//GENERATING C++: collect types\n";
  let types = ref [] in
  Hashtbl.iter (fun t index -> types := (index, t) :: !types) state.syms.registry;

  let types = List.sort (fun a1 a2 -> compare (fst a1) (fst a2)) !types in
  (*
  List.iter
  (fun (_,t) -> print_endline (string_of_btypecode sym_table t))
  types
  ;
  *)

  fprintf state.ppf "//GENERATING C++: type class names\n";
  plh "\n//-----------------------------------------";
  plh "//NAME THE TYPES";
  plh  (gen_type_names state.syms bsym_table types);

  fprintf state.ppf "//GENERATING C++: type class definitions\n";
  plh "\n//-----------------------------------------";
  plh  "//DEFINE THE TYPES";
  plh  (gen_types state.syms bsym_table types);

  fprintf state.ppf "//GENERATING C++: function and procedure classes\n";
  plh "\n//-----------------------------------------";
  plh  "//DEFINE FUNCTION CLASS NAMES";
  plh  (gen_function_names state.syms bsym_table);

  plh "\n//-----------------------------------------";
  plh  "//DEFINE FUNCTION CLASSES";
  plh  (gen_functions state.syms bsym_table);

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
  "  ::flx::gc::generic::gc_profile_t *gcp;";
  "  ::flx::gc::generic::gc_shape_t *shape_list_head;";
  "  thread_frame_t(";
  "  );";
  ]
  ;
  plh (format_vars state.syms bsym_table topvars []);
  plh "};";
  plh "";
  plh "FLX_DCL_THREAD_FRAME";
  plh "";
  plh ("}} // namespace flxusr::" ^ cid_of_flxid state.module_name);

  (* BODY *)
  fprintf state.ppf "//GENERATING C++: GC ptr maps & offsets\n";

  plb ("//Input file: " ^ state.input_filename);
  plb ("//Generated by Felix Version " ^ !version_data.version_string);
  plb ("//Timestamp: " ^ state.compile_start_gm_string);
  plb ("//Timestamp: " ^ state.compile_start_local_string);

  plb ("#include \"" ^ state.module_name ^ ".hpp\"");
  plb "#include <stdio.h>"; (* for diagnostics *)

  plb "#define comma ,";
  plb "\n//-----------------------------------------";
  plb "//EMIT USER BODY CODE";

  (* These must be in order: build a list and sort it *)
  instantiate_instances plb `Body;
  
  (* emit rtti file now so we can get the last_ptr_map and stick it
   * somewhere in the thread frame *)
  let last_ptr_map, tables = Flx_ogen.gen_offset_tables
    state.syms
    bsym_table
    state.module_name
    "&::flx::rtl::unit_ptr_map"
  in 
   plr tables
  ;


  plb "\n//-----------------------------------------";
  plb ("namespace flxusr { namespace " ^ cid_of_flxid state.module_name ^ " {");

  (* include RTTI file. Must be before the thread frame constructor because
   * the head of the rtti shape list may be thread_frame_t_ptr_map
   * which is stored in the thread frame so the library function can get
   * the shape object list *)

  ensure_closed state.rtti_file;
  if was_used state.rtti_file then begin
    plb "\n//-----------------------------------------";
    plb "//DEFINE OFFSET tables for GC";
    plb ("#include \"" ^ state.module_name ^ ".rtti\"");
  end;


  plb "FLX_DEF_THREAD_FRAME";
  plb "//Thread Frame Constructor";

  let sr = Flx_srcref.make_dummy "Thread Frame" in
  let topfuns = List.filter (fun (_,t) -> is_gc_pointer state.syms bsym_table sr t) topvars_with_type in
  let topfuns = List.map fst topfuns in
  let topinits =
    [
      "  gcp(0)";
      "  shape_list_head("^last_ptr_map^")";
    ]
    @
    List.map
    (fun index ->
      "  " ^
      cpp_instance_name state.syms bsym_table index [] ^
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
    state.syms.instances
    ;
    if !header_emitted then plb "#endif";
  end
  ;

  fprintf state.ppf "//GENERATING C++: method bodies\n";

  plb "\n//-----------------------------------------";
  plb "//DEFINE FUNCTION CLASS METHODS";
  plb ("#include \"" ^ state.module_name ^ ".ctors_cpp\"");
  gen_execute_methods
    state.body_file.out_filename
    state.syms
    bsym_table
    label_info
    state.syms.counter
    (force_open state.body_file)
    (force_open state.ctors_file);


  fprintf state.ppf "//GENERATING C++: interface\n";
  plb "\n//-----------------------------------------";
  plb ("}} // namespace flxusr::" ^ cid_of_flxid state.module_name);

  plb "//CREATE STANDARD EXTERNAL INTERFACE";
  plb ("FLX_FRAME_WRAPPERS(::flxusr::" ^ cid_of_flxid state.module_name ^ ")");
  (if List.mem `Pure topclass_props then
    plb ("FLX_C_START_WRAPPER(::flxusr::" ^ cid_of_flxid state.module_name ^ "," ^ top_class ^ ")")
  else if List.mem `Stackable topclass_props then
    plb ("FLX_STACK_START_WRAPPER(::flxusr::" ^ cid_of_flxid state.module_name ^ "," ^ top_class ^ ")")
  else
    plb ("FLX_START_WRAPPER(::flxusr::" ^ cid_of_flxid state.module_name ^ "," ^ top_class ^ ")")
  );
  plb "\n//-----------------------------------------";

  plh ("using namespace ::flxusr::" ^ cid_of_flxid state.module_name ^ ";");
(*
print_endline ("EXPORT COUNT = " ^ string_of_int (List.length state.syms.bifaces));
*)
  if List.length state.syms.bifaces > 0 then begin
    plh "//DECLARE USER EXPORTS";
    plh (gen_biface_headers state.syms bsym_table state.syms.bifaces);
    plb "//DEFINE EXPORTS";
    plb (gen_biface_bodies state.syms bsym_table state.syms.bifaces);
    plb (gen_python_module state.module_name state.syms bsym_table state.syms.bifaces);
  end
  ;

  (* rather late: generate variant remapping tables *)
  if Hashtbl.length state.syms.variant_map > 0 then begin
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
    state.syms.variant_map
  end
  ;
  plh "//header complete";
  plh "#endif";
  plb "//body complete";
  ensure_closed state.header_file;
  ensure_closed state.body_file;
  ensure_closed state.ctors_file;
  plp "flx";
  plp "flx_gc";  (* RF: flx apps now need flx_gc. is this the way to do it? *)
  ensure_closed state.package_file;

  state.codegen_time <- state.codegen_time +.  codegen_timer ();
  fprintf state.ppf "//code generation time %f\n" state.codegen_time


(** Save basic profiling numbers. *)
let save_profile state =
  let total_time =
    state.parse_time +.
    state.desugar_time +.
    state.bind_time +.
    state.opt_time +.
    state.instantiation_time +.
    state.codegen_time
  in
  fprintf state.ppf "//Felix compiler time %f\n" total_time;
  let fname = "flxg_stats.txt" in
  let
    old_parse_time,
    old_desugar_time,
    old_bind_time,
    old_opt_time,
    old_instantiation_time,
    old_codegen_time,
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
        with
        | Scanf.Scan_failure _
        | Failure _
        | End_of_file -> zeroes
      in close_in f; x
    end
  in

  (* *)
  (* failure to save stats isn't fatal *)
  try
    let f = open_out fname in
    Printf.fprintf
      f
      "parse=%f\ndesugar=%f\nbind=%f\nopt=%f\ninst=%f\ngen=%f\ntot=%f\n"
      (old_parse_time +. state.parse_time)
      (old_desugar_time +. state.desugar_time)
      (old_bind_time +. state.bind_time)
      (old_opt_time +. state.opt_time)
      (old_instantiation_time +. state.instantiation_time)
      (old_codegen_time +. state.codegen_time)
      (old_total_time +. total_time)
    ;
    close_out f
  with _ -> ()

(* Desugar takes a parse tree, which is an Ocaml list of STMT_* s
   and produces a list of include file names and
   a list of assmebly instructions. The only state used is a fresh
   id generator. 
*)

(* make_assembly parses and desugars the transitive closure of the specified
   input file with respect to includes,  and returns a list of pairs
   mapping files to assembly lists. 

   The name specified is only used by the macroprocessor to create unique
   names. (?)
*)

let strip_extension s =
  let n = String.length s in
  if n>4 then 
    if String.sub s (n-4) 4 = ".flx" 
    then String.sub s 0 (n-4)
    else s
  else s

type include_entry_t = Search of string | NoSearch of string

(* if an including_file_base "a/b" in including_file_dir "c/d" includes
   with string 
   "f" then we use include entry Search "f" (search the path)
   "./f" then we use NoSearch "c/d/a/f" (sibling of including file)
*)

let make_include_entry including_file_dir including_file_base  include_string =
  let n = String.length include_string in
  if n > 1 then
    if String.sub include_string 0 2 = "./" 
    then begin 
      let dirpart = Filename.dirname including_file_base in
      let dirpart = if dirpart = "." then "" else dirpart in
      let dir = Flx_filesys.join including_file_dir dirpart  in
      let basepart = String.sub include_string 2 (n-2) in
      let fullname = Flx_filesys.join dir basepart in
      NoSearch fullname
    end else Search include_string 
  else Search include_string


(* filename: include string, with leading . replaced
 * depname: the complete pathname
*)
type ub_entry_t = { filename:string;  depname:string; asms: asm_t list }


(* make_assembly does NOT modify anything at all in the symbol state
 * except for the fresh bid counter *)

(* exclusions: if an include was of the form "x/y",
   then exclusions has the string "x/y".
   if an include was from "d/a", and of the form "./x/y"
   then exclusions has the string "d/x/y"
   This form is thus relative to the current search path directory:
   the sibling reference is factored out, but the form is invariant
   over search paths. So if the Felix installation is moved this form
   is invariant: we have to put this in the libtab cache so it is mobile.
   However the *.dep file generated requires fully resolved path names.

   This means "make_include_entry" should return only the canonical form,
   and NOT the resolved filename, which means it has to be called with
   the including_file_dir also being a canonical string and not a resolved
   filename, however that won't work, because NoSearch doesn't search.

*)

let make_assembly 
  state 
  parser_state
  (exclusions:string list)  
  (module_name:string) 
  (input:include_entry_t) 
  : ub_entry_t list
=
(*
print_endline ("Exclusions = " ^ String.concat ", " exclusions);
*)
    let root = 0 in
    let counter_ref = state.syms.counter in
    let fresh_bid () = Flx_mtypes2.fresh_bid counter_ref in
    let print_flag = state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag in
    let outputs = ref [] in

    (* PARSE THE IMPLEMENTATION FILES *)
    let processed = ref exclusions in
    let unprocessed = ref [input] in

    let outdir= state.syms.compiler_options.cache_dir in
    while List.length (!unprocessed) > 0 do

      (* pop a candidate *)
      let candidate = List.hd (!unprocessed) in
      unprocessed := List.tl (!unprocessed);

      (* resolve the filename *)
      let filedir,filename = match candidate with
        | Search s ->
          Flx_filesys.find_include_dir 
          ~include_dirs:state.syms.compiler_options.include_dirs
          (s ^ ".flx"),s
        | NoSearch s -> "",s
      in
      let flx_base_name = Flx_filesys.join filedir filename in
     
(*
let _ = print_endline ("filedir=" ^ filedir) in
let _ = print_endline ("filename=" ^ filename) in
let _ = print_endline ("flx_base_name= " ^ flx_base_name) in
let _ = print_endline ("Processed = " ^ String.concat ", " (!processed)) in
*)
      (* check if already processed *)
      if not (List.mem filename (!processed)) then
      begin
        (* flag already processed *)
        processed := filename :: !processed;

        (* check the felix file modification time *)
        let flx_name = flx_base_name ^ ".flx" in
        let flx_time = Flx_filesys.virtual_filetime Flx_filesys.big_crunch flx_name in

        (* get the parse of the felix file, with caching  *)
        let stmts = 
(* let _ = print_endline ("Filename is " ^ flx_name) in *)
          let in_par_name = Flx_filesys.join filedir filename ^ ".par2" in
          let out_par_name = 
             match outdir with 
             | Some d -> Some (Flx_filesys.join d filename ^ ".par2")
             | None -> None
          in
          let stmts = Flx_filesys.cached_computation "parse" in_par_name
            ~outfile:out_par_name
            ~min_time:flx_time
            (fun () -> parse_file state parser_state flx_name)
          in
          stmts
        in

        (* Desugare the parse tree, and also return list of include strings *)
        let include_files, asms =  
          let desugar_state = Flx_desugar.make_desugar_state module_name fresh_bid in
          let include_files, asms = Flx_desugar.desugar_stmts desugar_state (Filename.dirname filename) stmts in
          let top_req = 
            let sr = Flx_srcref.dummy_sr in
            let body = Flx_types.DCL_insert (
              CS.Str "",
              `Body,
              Flx_ast.NREQ_true)
            in
            Flx_types.Dcl (
              sr,
              "_rqs_" ^ module_name,
              None,
              `Public,
              Flx_ast.dfltvs,
              body)
          in
          let asms = top_req::asms in
          include_files, asms 
        in

        (* run through include strings found *)
        List.iter (fun s -> 
          (* Convert include string to an include term *)
          let include_entry = make_include_entry filedir filename s in

          (* add the name to the unprocessed include list, even if already processed 
           * if it is already processed that will be found when it is resolved at
           * the top of this loop *)
          if not (List.mem include_entry (!unprocessed)) 
          then begin
(*            print_endline ("Add Include file: " ^ s); *)
            unprocessed := include_entry :: (!unprocessed)
          end
        ) 
        (* desugar outputs the include files backwards, but we push them onto the unprocessed
         * stack which means the order is again reversed so we actually get the proper order
         * of initialisation *)
        include_files
        ;
        (* add record for processed file *)
        outputs := {filename=filename; depname=flx_base_name; asms=asms;} :: (!outputs);
     end
    done;
    (* but again, the order is reversed here *)
    !outputs

let process_lib state parser_state sym_table_ref bsym_table_ref excls outdir module_name start_counter lib =
(*
      print_endline ("Processing library " ^ h);
*)
  let lib_filedir,lib_filename = 
      Flx_filesys.find_include_dir 
       ~include_dirs:state.syms.compiler_options.include_dirs
      (lib ^ ".flx"),lib
  in
  let lib_name = Flx_filesys.join lib_filedir (lib_filename ^ ".flx") in

  (* this is wrong, should be he max of the included file times, but until
   * we load the cached value we don't know what files are included.
   * To fix this we could use a separate dep file, or simply validate
   * the file after it is loaded. Of course to do that I have to put
   * the include file list into the cache!
   *)
  let lib_time = Flx_filesys.virtual_filetime Flx_filesys.big_crunch lib_name in
  let in_libtab_name = Flx_filesys.join lib_filedir lib_filename ^ ".libtab" in
  let out_libtab_name = 
    match outdir with 
    | Some d -> Some (Flx_filesys.join d lib_filename ^ ".libtab")
    | None -> None
  in
  let lib_cache_time = Flx_filesys.virtual_filetime Flx_filesys.big_bang in_libtab_name in
  let validate (_,depnames,_,_,_,_,_,_,_,_,_) = 
    let filetimes = List.fold_left (fun acc f -> 
      (*
      print_endline ("Depfile=" ^ f); 
      *)
      max acc (Flx_filesys.virtual_filetime Flx_filesys.big_crunch (f^".flx"))) 
      Flx_filesys.big_bang depnames 
    in
    (*
    print_endline ("Cached include filetimes = " ^ string_of_float filetimes);
    print_endline ("Libtime = " ^ string_of_float lib_time);
    *)
    let valid = filetimes < lib_cache_time in
    (*
    print_endline (if valid then "libtab is still valid" else "libtab is out of date");
    *)
    valid
  in
  let 
    includes, depnames,
    saved_counter, 
    varmap, 
    ticache, 
    typeclass_to_instance, 
    instances_of_typeclass, 
    axioms, 
    reductions,
    out_sym_table,
    out_bsym_table 
  =
    Flx_filesys.cached_computation "libtab" in_libtab_name
      ~outfile:out_libtab_name
      ~force_calc:(false || state.syms.compiler_options.force_recompile)
      ~min_time:lib_time 
      ~validate
      (fun () -> 
        (* make assembly outputs stuff in reversed order, but this routine
         * reversed it back again *)
        let cal_time = make_timer() in
        print_endline ("Binding libary " ^ lib);
        let assembly = make_assembly state parser_state !excls lib (Search lib) in
        let includes, depnames, asmss= 
           let rec aux includes depnames asmss a = match a with
           | [] -> includes, depnames, asmss
           | { filename=filename; depname=depname; asms=asms; } :: t ->
               aux (filename :: includes) (depname::depnames) (asms::asmss) t
           in aux [] [] [] assembly
        in
        (* already in the right order now *)
        let asms = List.concat asmss in
        let asms = make_module module_name asms in
        (*
        print_endline "Binding asms: ";
        List.iter (fun a -> print_endline (Flx_print.string_of_asm 2 a)) asms;
        *)
        (* Bind the assemblies. *)
        bind_asms state !sym_table_ref !bsym_table_ref !start_counter asms;
        print_endline ("binding library " ^ lib ^ " done in "^ string_of_float (cal_time()) ^ " seconds");
        print_endline ("Exports = " ^ string_of_int (List.length (state.syms.bifaces)));
        includes, depnames,
        !(state.syms.counter),
        state.syms.varmap, 
        state.syms.ticache, 
        state.syms.typeclass_to_instance, 
        state.syms.instances_of_typeclass, 
        !(state.syms.axioms), 
        !(state.syms.reductions),
        !sym_table_ref,
        !bsym_table_ref 
      )
  in
  state.syms.counter := max !(state.syms.counter)  saved_counter;
  state.syms.varmap <- varmap;
  state.syms.ticache <- ticache;
  state.syms.typeclass_to_instance <- typeclass_to_instance;
  state.syms.instances_of_typeclass <- instances_of_typeclass;
  state.syms.axioms := axioms;
  state.syms.reductions := reductions;
  start_counter := !(state.syms.counter);
  sym_table_ref := out_sym_table;
  bsym_table_ref := out_bsym_table;

  (* already processed include files *)
  excls := includes @ !excls


let main () =
  let start_counter = ref 2 in
  let ppf, compiler_options = parse_args () in
  let state = make_flxg_state ppf compiler_options in
(*
print_endline ("Include dirs=" ^ String.concat ", " compiler_options.include_dirs);
*)
  let module_name = 
     try make_module_name (List.hd (List.rev (state.syms.compiler_options.files)))
     with _ -> "empty_module"
  in
  let bsym_table_ref = ref (Flx_bsym_table.create ()) in
  let sym_table_ref = ref (Flx_sym_table.create ()) in
  let sym : Flx_sym.t = {
    Flx_sym.id="root"; 
    sr=Flx_srcref.dummy_sr; 
    vs=Flx_ast.dfltvs;
    pubmap=Hashtbl.create 97; 
    privmap=Hashtbl.create 97; 
    dirs=[]; 
    symdef=(SYMDEF_root []) 
  } 
  in
  let root_elt = {Flx_sym_table.parent=None; sym=sym; } in
  Hashtbl.add (!sym_table_ref) 0 root_elt;

  let inroots = List.rev state.syms.compiler_options.files in (* reverse order of command line *)
  let outdir= state.syms.compiler_options.cache_dir in
  (* print_endline ("Inputs=" ^ String.concat ", " inroots); *)
  begin try
    if List.length inroots = 0 then
      raise (Failure "No input files on comamnd line")
    ;
    let parser_state = load_syntax state in

    let main_prog = List.hd inroots in
    let libs = List.rev (List.tl inroots) in
(*
    print_endline ("Libraries=" ^ String.concat ", " libs);
    print_endline ("Main program =" ^ main_prog);
*)
    let excls:string list ref = ref [] in
    List.iter (process_lib state parser_state sym_table_ref bsym_table_ref excls outdir module_name start_counter) libs;

(*
    print_endline ("Making symbol tables for main program " ^ main_prog);
    print_endline ("Excludes: " ^ String.concat ", " (!excls));
*)
    let sym_table = !sym_table_ref in
    let bsym_table = !bsym_table_ref in
    let assembly =  make_assembly state parser_state !excls module_name (NoSearch main_prog) in
    let includes, depnames, asmss= 
       let rec aux includes depnames asmss a = match a with
       | [] -> includes, depnames, asmss
       | { filename=filename; depname=depname; asms=asms; } :: t ->
       aux (filename :: includes) (depname::depnames) (asms::asmss) t
       in aux [] [] [] assembly
    in
    let asms = List.concat (List.rev asmss) in

    (* update the global include file list *)
    state.syms.include_files := depnames;
    generate_dep_file state;

(*
print_endline "DEBUG: include files are:";
List.iter print_endline !(state.syms.include_files);
*)

    let asms = make_module module_name asms in
(*
print_endline "Binding main program asms";
*)
(*
List.iter (fun a -> print_endline (Flx_print.string_of_asm 2 a)) asms;
*)
    (* Bind the assemblies. *)
(*
print_endline "sym_table=";
print_endline (Flx_sym_table.detail sym_table);
*)
    bind_asms state sym_table bsym_table (!start_counter) asms;
(*
print_endline ("Main prog: Exports = " ^ string_of_int (List.length (state.syms.bifaces)));
*)
    start_counter := !(state.syms.counter);
(*
print_endline "Main program bound";
*)
    (* make the root proc *)

    let entry = try Hashtbl.find sym_table 0 with Not_found -> failwith "flxg: can't find root" in
    let sym = match entry with
      | { Flx_sym_table.parent=None; sym=sym } -> sym
      | _ -> failwith "flxg: expected root entry to have parent None"
    in 
    let exes = match sym with
    | {Flx_sym.symdef=SYMDEF_root exes} -> exes
    | _ -> failwith "flxg: expected root entry to be SYMDEF_root"
    in
    let asms = List.map (fun x -> Exe x) exes in
    (* this is a hack .. oh well .. *)
    let root_proc = Flx_mtypes2.fresh_bid (state.syms.counter) in
    let dcl = DCL_function (
      ([],None),
      Flx_ast.TYP_void Flx_srcref.dummy_sr,
      [],
      asms)
    in
    let asm = Dcl (
      Flx_srcref.dummy_sr,
      "_init_",
      Some root_proc,
      `Public,
      Flx_ast.dfltvs,
      dcl)
    in
    let asms = make_module module_name [asm] in
(*
print_endline ("Binding init proc " ^ string_of_int root_proc);
*)
    bind_asms state sym_table bsym_table root_proc asms;
(*
print_endline ("Init proc: Exports = " ^ string_of_int (List.length (state.syms.bifaces)));
*)
(*
print_endline "init proc bound";
*)
    if not (Flx_sym_table.mem sym_table root_proc) then
      failwith "flxg: can't find init proc in unbond symbol table "
    ;
    if not (Flx_bsym_table.mem bsym_table root_proc) then
      failwith "flxg: can't find init proc in bound symbol table"
    ;

    Flx_typeclass.typeclass_instance_check state.syms bsym_table;

  (* generate axiom checks *)
  (* or not: the routine must be run to strip axiom checks out of the code *)
  Flx_axiom.axiom_check state.syms bsym_table
    state.syms.compiler_options.generate_axiom_checks
  ;

(* Not working at the moment for unknown reason, chucks Not_found 
    (* Generate the why file *)
    generate_why_file state bsym_table root_proc;
print_endline "Why file generated";
*)

(*
    (* Remove unused symbols. *)

    (* THIS DOESN'T WORK. WHY NOT? Seems like newtype isn't scanned
       properly. No idea why! After downgrading, optimise does this
       first thing, so, it has to be a problem with BBDCL_newtype!

       AH. I know. The scan is finding the newtype index, but it 
       isn't propagating that to the representation .. wonder why?

       I mean, this HAS to work for say, structs.
    *)
    let bsym_table = Flx_use.copy_used state.syms bsym_table in
*)
    (* Optimize the bound values *)
(*
print_endline "starting optimisation";
*)
    let bsym_table = optimize_bsyms state bsym_table root_proc in
(*
print_endline "Optimisation complete";
*)
    (* downgrade abstract types now *)
    Flx_strabs.strabs (Flx_strabs.make_strabs_state ()) bsym_table; 
(*
print_endline "Strabs complete";
*)

(* Lower the bound symbols for the backend. *)
    let bsym_table = lower_bsyms state bsym_table root_proc in
(*
print_endline "Lowering abstract types complete";
*)
    (* Start working on the backend. *)
    codegen_bsyms state bsym_table root_proc
(*
; print_endline "Code gen complete"
*)
  with x ->
    Flx_terminate.terminate compiler_options.reverse_return_parity x
  end;

  (* We're done! let's calculate some simple profile statistics. *)
  save_profile state;

  if compiler_options.reverse_return_parity then 1 else 0
;;

exit (main ())
