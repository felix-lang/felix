(* code generation driver *)

open Format

open Flx_util
open Flx_ast
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
let parse_file state file =
  let parse_timer = make_timer () in

  let file_name =
    if Filename.check_suffix file ".flx" then file else file ^ ".flx"
  in
  let local_prefix = Filename.chop_suffix (Filename.basename file_name) ".flx" in
  let include_dirs = state.syms.compiler_options.include_dirs in
  fprintf state.ppf "//Parsing Implementation %s\n" file_name;
  if state.syms.compiler_options.print_flag then print_endline ("Parsing " ^ file_name);
  let parser_state = List.fold_left
    (Flx_parse.parse_file ~include_dirs)
    (Flx_parse.make_parser_state (fun stmt stmts -> stmt :: stmts) [])
    (state.syms.compiler_options.auto_imports @ [file_name])
  in
  let stmts = List.rev (Flx_parse.parser_data parser_state) in
  let macro_state = Flx_macro.make_macro_state local_prefix in
  let stmts = Flx_macro.expand_macros macro_state stmts in

  state.parse_time <- state.parse_time +. parse_timer ();

  stmts


(** Desugar the statements *)
let make_module module_name asms =
  let asms =
    [Dcl (Flx_srcref.dummy_sr, module_name, None, `Public, dfltvs, DCL_module asms)]
  in
  asms


(** Bind the assemblies *)
let bind_asms state asms =
  fprintf state.ppf "//BINDING\n";
  let bind_timer = make_timer () in

  (* THIS IS A HACK! *)
  let root = !(state.syms.counter) in
  fprintf state.ppf "//Top level module '%s' has index %s\n"
    state.module_name
    (string_of_bid root);

  let bind_state = Flx_bind.make_bind_state state.syms in
  let bsym_table = Flx_bsym_table.create () in
  Flx_bind.bind_asms bind_state bsym_table asms;

  let root_proc = Flx_bind.find_root_module_init_function bind_state root in
  fprintf state.ppf "//root module's init procedure has index %s\n"
    (Flx_print.string_of_bid root_proc);

  Flx_typeclass.typeclass_instance_check state.syms bsym_table;

  state.bind_time <- state.bind_time +. bind_timer ();
  fprintf state.ppf "//BINDING OK time %f\n" state.bind_time;

  bsym_table, root_proc


(** Generate the why file. *)
let generate_why_file state bsym_table root_proc =
  (* generate axiom checks *)
  if state.syms.compiler_options.generate_axiom_checks then
  Flx_axiom.axiom_check state.syms bsym_table;

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

  (* These must be in order: build a list and sort it *)
  begin
    let dfnlist = ref [] in
    Hashtbl.iter
    (fun (i,ts) _ -> dfnlist := (i,ts) :: !dfnlist)
    state.syms.instances
    ;
    let insts = Hashtbl.create 97 in
    List.iter
    (fun (i,ts)->
      let bsym =
        try Flx_bsym_table.find bsym_table i with Not_found ->
          failwith ("[package] can't find index " ^ string_of_bid i)
      in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_external_code (_,s,`Package,_) ->
        begin match s with
        | CS_identity | CS_str "" | CS_str_template "" -> ()
        | _ ->
          let s =
            match s with
            | CS_identity -> assert false (* covered above *)
            | CS_virtual -> clierr (Flx_bsym.sr bsym) "Instantiate virtual insertion!"
            | CS_str s -> Flx_cexpr.ce_expr "atom" s
            | CS_str_template s ->
              (* do we need tsubst vs ts t? *)
              let tn t = cpp_typename state.syms bsym_table t in
              let ts = List.map tn ts in
              Flx_csubst.csubst (Flx_bsym.sr bsym) (Flx_bsym.sr bsym) s (Flx_cexpr.ce_atom "Error") [] [] "Error" "Error" ts "atom" "Error" ["Error"] ["Error"] ["Error"]
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
  plh "using namespace ::flx::rtl;"; 
  plh "#include \"flx_gc.hpp\"";
  plh "using namespace ::flx::gc::generic;"; 

  plh "#ifndef FLX_NO_INCLUDES";
  plh ("#include \"" ^ state.module_name ^ ".includes\"");
  plh "#endif";
  plh "";

  plh "\n//-----------------------------------------";
  plh "//USER HEADERS";
  (* These must be in order: build a list and sort it *)
  begin
    let dfnlist = ref [] in
    Hashtbl.iter
    (fun (i,ts) _ -> dfnlist := (i,ts) :: !dfnlist)
    state.syms.instances
    ;
    let insts = Hashtbl.create 97 in
    List.iter
    (fun (i,ts)->
      let bsym =
        try Flx_bsym_table.find bsym_table i with Not_found ->
          failwith ("[user header] can't find index " ^ string_of_bid i)
      in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_external_code (_,s,`Header,_) ->
        begin match s with
        | CS_identity | CS_str "" | CS_str_template "" -> ()
        | _ ->
          let s =
            match s with
            | CS_identity -> assert false
            | CS_virtual -> clierr (Flx_bsym.sr bsym) "Instantiate virtual insertion!"
            | CS_str s -> Flx_cexpr.ce_expr "atom" s
            | CS_str_template s ->
              (* do we need tsubst vs ts t? *)
              let tn t = cpp_typename state.syms bsym_table t in
              let ts = List.map tn ts in
              Flx_csubst.csubst (Flx_bsym.sr bsym) (Flx_bsym.sr bsym) s (Flx_cexpr.ce_atom "Error") [] [] "Error" "Error" ts "atom" "Error" ["Error"] ["Error"] ["Error"]
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
  "  gc_profile_t *gcp;";
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
  begin
    let dfnlist = ref [] in
    Hashtbl.iter
    (fun (i,ts) _ -> dfnlist := (i,ts) :: !dfnlist)
    state.syms.instances
    ;
    let insts = Hashtbl.create 97 in
    List.iter
    (fun (i,ts) ->
      let bsym =
        try Flx_bsym_table.find bsym_table i with Not_found ->
          failwith ("[user body] can't find index " ^ string_of_bid i)
      in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_external_code (_,s,`Body,_) ->
        begin match s with
        | CS_identity | CS_str "" | CS_str_template "" -> ()
        | _ ->
          let s =
            match s with
            | CS_identity -> assert false
            | CS_virtual -> clierr (Flx_bsym.sr bsym) "Instantiate virtual insertion!"
            | CS_str s -> Flx_cexpr.ce_expr "atom" s
            | CS_str_template s ->
              (* do we need tsubst vs ts t? *)
              let tn t = cpp_typename state.syms bsym_table t in
              let ts = List.map tn ts in
              Flx_csubst.csubst (Flx_bsym.sr bsym) (Flx_bsym.sr bsym) s (Flx_cexpr.ce_atom "Error") [] [] "Error" "Error" ts "atom" "Error" ["Error"] ["Error"] ["Error"]
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
  plb ("namespace flxusr { namespace " ^ cid_of_flxid state.module_name ^ " {");

  plb "FLX_DEF_THREAD_FRAME";
  plb "//Thread Frame Constructor";

  let sr = Flx_srcref.make_dummy "Thread Frame" in
  let topfuns = List.filter (fun (_,t) -> is_gc_pointer state.syms bsym_table sr t) topvars_with_type in
  let topfuns = List.map fst topfuns in
  let topinits =
    [
      "  gcp(0)"
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

  plr (Flx_ogen.gen_offset_tables
    state.syms
    bsym_table
    state.module_name);

  ensure_closed state.rtti_file;
  if was_used state.rtti_file then begin
    plb "\n//-----------------------------------------";
    plb "//DEFINE OFFSET tables for GC";
    plb ("#include \"" ^ state.module_name ^ ".rtti\"");
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

type include_entry_t = Search of string | NoSearch of string

let make_assembly 
  state 
  (exclusions:include_entry_t list)  
  (module_name:string) 
  (input:include_entry_t) 
  : ((include_entry_t * string)* Flx_types.asm_t list) list
=
    let root = 0 in
    let counter_ref = state.syms.counter in
    let fresh_bid () = Flx_mtypes2.fresh_bid counter_ref in
    let print_flag = state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag in
    let outputs = ref [] in
    (* PARSE THE IMPLEMENTATION FILES *)
    let processed = ref exclusions in
    let unprocessed = ref [input] in
(* print_endline ("Initial files are " ^ String.concat "," (!unprocessed)); *)

    (* This algorithm is bugged! When desugaring finds a nested module,
       it generated a call to the modules initialisation function.
       So if we paste an include file into the parse tree somewhere,
       it usually contains a module, and that module's initialisation
       will be called: since includes go at the top of the file,
       it will be done before assigning to any of ihe current files vars.
    *)
    let strip_extension s =
      let n = String.length s in
      if n>4 then 
        if String.sub s (n-4) 4 = ".flx" 
        then String.sub s 0 (n-4)
        else s
      else s
    in
    let outdir= state.syms.compiler_options.cache_dir in
    while List.length (!unprocessed) > 0 do
      let candidate = List.hd (!unprocessed) in
(*      let candidate = strip_extension candidate in *)
      unprocessed := List.tl (!unprocessed);
      if not (List.mem candidate (!processed)) then
      begin
        let filedir,filename = match candidate with
          | Search s ->
            Flx_filesys.find_include_dir 
             ~include_dirs:state.syms.compiler_options.include_dirs
            (s ^ ".flx"),s
          | NoSearch s -> "",s
        in
        let flx_name = Flx_filesys.join filedir (filename ^ ".flx") in
        let flx_time = Flx_filesys.virtual_filetime Flx_filesys.big_crunch flx_name in
        processed := candidate :: !processed;
        let stmts = 
(* print_endline ("Filename is " ^ flx_name); *)
          let in_par_name = Flx_filesys.join filedir filename ^ ".par2" in
          let out_par_name = 
             match outdir with 
             | Some d -> Some (Flx_filesys.join d filename ^ ".par2")
             | None -> None
          in
          let stmts = Flx_filesys.cached_computation "parse" in_par_name
            ~outfile:out_par_name
            ~min_time:flx_time
            (fun () -> parse_file state flx_name)
          in
          stmts
        in
        let include_files, asms =  
          let desugar_state = Flx_desugar.make_desugar_state module_name fresh_bid in
          Flx_desugar.desugar_stmts desugar_state (Filename.dirname filename) stmts 
        in
        let in_tab_name = Flx_filesys.join filedir filename ^ ".symtab" in
        let out_tab_name = 
             match outdir with 
           | Some d -> Some (Flx_filesys.join d filename ^ ".symtab")
           | None -> None
        in
(*
print_endline ("Loading symbol tables for " ^ filename);
*)
        let symtab= Flx_filesys.cached_computation "symtab" in_tab_name
          ~outfile:out_tab_name
          ~min_time:flx_time
          (fun () -> 
(*
print_endline ("Attempting to make symbol tables for " ^ filename);
*)
          let symbol_table = Flx_sym_table.create () in
          (* A symtab is a symbol table together with public and private name mappings.
           * Unfortunately also captures the top level state object.
           *)
          let symtab = Flx_symtab.make symbol_table in
          Flx_symtab.add_asms 
            print_flag counter_ref 
            symtab   (* the indicies *)
            filename (* containing module name *)
            1        (* nesting level, a hack! *)
            None     (* parent module, to be root *)
            root     (* root module index, 0 *)
            asms     (* Stuff to add *)
           ;
(*
print_endline ("Made symbol tables for " ^ filename);
*)
           symtab
           )
        in 
print_endline ("Table for " ^ filename ^ " " ^ Flx_symtab.detail symtab );

        List.iter (fun s -> 
          (* Grr.. design fault here. If the filename is ./fred.flx then we're
           * and it is included by a/b.flx then we're talking about a/fred.flx
           * Unfortunately, include files are being searched for, so what do
           * we put? We want to *stop* the search in this case and use:
           * filedir "/" dirname including_file "/" included_file[2:] ".flx"
           *)
          let s = 
            let n = String.length s in
            if n > 1 then
              if String.sub s 0 2 = "./" 
              then begin 
                let dirpart = Filename.dirname filename in
                let dirpart = if dirpart = "." then "" else dirpart in
                let dir = Flx_filesys.join filedir dirpart  in
                let basepart = String.sub s 2 (n-2) in
                let fullname = Flx_filesys.join dir basepart in
                NoSearch fullname
              end else Search s
            else Search s
          in
          if not (List.mem s (!processed)) && not (List.mem s (!unprocessed)) 
          then begin
(*            print_endline ("Add Include file: " ^ s); *)
            unprocessed := s :: (!unprocessed)
          end
        ) 
        (List.rev include_files)
        ;
        (* the list is order of writing?
           We reverse it.
           The loop reverses it.
           reversed yet a third time when the result
           is pushed onto the output stack .. so we're back to
           order of writing.
        *)

        outputs := ((candidate,filedir),asms) :: (!outputs)
     end
    done;
    !outputs

let main () =
  let ppf, compiler_options = parse_args () in
  let state = make_flxg_state ppf compiler_options in
  let module_name = 
     try make_module_name (List.hd state.syms.compiler_options.files)
     with _ -> "empty_module"
  in

  let pub_name_map = Hashtbl.create 97 in
  let priv_name_map = Hashtbl.create 97 in
  let sym_table = Hashtbl.create 97 in
  let inherit_ivs = [] in
  let level = 0 in
  let parent = Some 0 in
  let root = 0 in

  let outputs = ref [] in
  let inroots = List.rev state.syms.compiler_options.files in (* reverse order of command line *)
  (* print_endline ("Inputs=" ^ String.concat ", " inroots); *)
  begin try
    if List.length inroots = 0 then
      raise (Failure "No input files on comamnd line")
    ;
    let main_prog = List.hd inroots in
    let libs = List.rev (List.tl inroots) in
    (* print_endline ("Libraries=" ^ String.concat ", " libs); *)
    (* print_endline ("Main program =" ^ main_prog); *)

    let rec aux exclusions ls = match ls with 
    | [] -> ()
    | h :: t -> 
       (* print_endline ("Processing library " ^ h); *)
       let assembly = make_assembly state exclusions h (Search h) in
       outputs := assembly @ (!outputs);
       let exclusions = fst (List.split (fst (List.split assembly))) @ exclusions in
       aux exclusions t
    in
    aux [] libs;
 
    (* print_endline ("Making symbol tables for main program " ^ main_prog); *)
    let exclusions = fst (List.split (fst (List.split (!outputs)))) in
    let assembly =  make_assembly state exclusions module_name (NoSearch main_prog) in
    outputs := (!outputs) @ assembly;

    generate_dep_file state;

(*
print_endline "DEBUG: include files are:";
List.iter print_endline !(state.syms.include_files);
*)

    let asms = List.concat (snd (List.split (!outputs))) in
    let asms = make_module module_name asms in
    (* Bind the assemblies. *)
    let bsym_table, root_proc = bind_asms state asms in

    (* Generate the why file *)
    generate_why_file state bsym_table root_proc;

    (* Optimize the bound values *)
    let bsym_table = optimize_bsyms state bsym_table root_proc in

    (* Lower the bound symbols for the backend. *)
    let bsym_table = lower_bsyms state bsym_table root_proc in

    (* Start working on the backend. *)
    codegen_bsyms state bsym_table root_proc;
  with x ->
    Flx_terminate.terminate compiler_options.reverse_return_parity x
  end;

  (* We're done! let's calculate some simple profile statistics. *)
  save_profile state;

  if compiler_options.reverse_return_parity then 1 else 0
;;

exit (main ())
