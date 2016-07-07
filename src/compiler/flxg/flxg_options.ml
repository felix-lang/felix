open Flx_getopt
open Flx_mtypes2
open Flx_options
open Flx_set
open Flx_version

(* this stupid routine gets rid of all duplicate // in a filename, and also
 * any trailing /, since for some reason this confuses the include file
 * exclusion checks
 *)
let fixit file =
  let n = String.length file in
  let s = ref "" in
  let copy = ref true in
  for i = 0 to n-1 do
    let ch = String.sub file i 1 in
    if ch <> Flx_filesys.dir_sep then copy := true;
    if !copy then s := !s ^ ch;
    copy := ch <> Flx_filesys.dir_sep
  done;
  let s = !s in
  let n = String.length s in
  let s =
    if n>1 then
      if String.sub s (n-1) 1 = Flx_filesys.dir_sep
      then String.sub s (n-1) 1
      else s
    else s
  in
  s


let fixup files = List.map fixit files


let get_options raw_options =
  let compiler_phase =
    match get_key_value raw_options "compiler-phase" with
    | None -> Phase_codegen
    | Some phase ->
        match phase with
        | "parse" -> Phase_parse
        | "desugar" -> Phase_desugar
        | "bind" -> Phase_bind
        | "optimise" -> Phase_optimize
        | "optimize" -> Phase_optimize
        | "lower" -> Phase_lower
        | "codegen" -> Phase_codegen
        | _ -> failwith "Invalid value for --compiler-phase"
  in
  let include_dirs= fixup (get_keys_values raw_options ["I"; "include"]) in
  let include_dirs = List.map (fun s -> Flx_filesys.mkabs s) include_dirs in
  let options =
  {
    compiler_phase = compiler_phase;
    optimise    = check_keys raw_options ["opt"; "optimise";"optimize"];
    doreductions = not (check_key raw_options "no-reduce");
    debug       = check_key raw_options "debug";
    with_comments = check_key raw_options "with-comments";
    mangle_names = check_key raw_options "mangle-names";
    include_dirs= include_dirs;
    print_flag  = check_keys raw_options ["v"; "verbose"];
    generate_axiom_checks  = not (check_keys raw_options ["no-check-axioms"]);
    trace       = check_keys raw_options ["trace"];
    files       = List.rev (get_key_values raw_options "");
    raw_options = raw_options;
    reverse_return_parity = check_key raw_options "e";
    force_recompile = check_keys raw_options ["force"];
    cache_dir = Flx_filesys.mkabs (match get_key_value raw_options "cache_dir" with Some x -> x | None -> Flx_filesys.root_dir);
    output_dir = Flx_filesys.mkabs (match get_key_value raw_options "output_dir" with Some x -> x | None -> Flx_filesys.root_dir);
    bundle_dir = get_key_value raw_options "bundle_dir";
    max_inline_length =
      begin
        let inline =
          match get_key_value raw_options "inline" with
          | Some i ->
            (
              if i = "none" then 0 else
              if i = "" then 150 else
                try
                  int_of_string i
                with _ ->
                  failwith ("Invalid value for inline: '" ^ i^"'")
            )
          | None ->
            begin match check_key raw_options "noinline" with
            | true -> 0
            | false ->
              begin match check_keys raw_options ["inline";"opt";"optimise"] with
              | true -> 250
              | false -> 15
              end
            end
        in
          (* we need to at least inline a little for typeclasses *)
          if inline < 10 then 10 else inline
      end
    ;
    auto_imports = get_key_values raw_options "import";
    syntax= get_key_values raw_options "syntax";
    automaton_filename=""; 
    compile_only = check_keys raw_options ["c";"compile-only"];
    showtime = check_keys raw_options ["time"]
  }
  in
  let automaton =
      begin match get_key_value raw_options "automaton" with
      | Some a -> Flx_filesys.mkabs a
      | None ->
        begin 
          try 
            Flx_filesys.find_file ~include_dirs "syntax.automaton" 
          with Flx_filesys.Missing_path _ -> 
          try 
            let d = Flx_filesys.find_dir ~include_dirs "grammar" in
            Flx_filesys.mk_cache_name options.cache_dir (Flx_filesys.join d "syntax.automaton")
          with Flx_filesys.Missing_path _ -> Flx_filesys.mk_cache_name options.cache_dir "syntax.automaton" 
        end
      end
    ;
  in
  let options = { options with automaton_filename = automaton } in
(*
  print_endline ("Cache_dir= " ^ options.cache_dir);
  print_endline ("Output_dir= " ^ options.output_dir);
  print_endline ("Include_dirs= " ^ String.concat ", " options.include_dirs);
  print_endline ("Syntax_automaton= " ^ options.automaton_filename);
  print_endline ("Auto_imports= " ^ String.concat ", " options.auto_imports);
  print_endline ("Syntax = " ^ String.concat ", " options.syntax);
  print_endline ("Files = " ^ String.concat ", " options.files);
*)
  options


let print_options () =
  print_endline "options:";
  print_endline "  -h, --help : print this help";
  print_endline "  --version: print version info";
  print_endline "  -v, --verbose: print symbol table";
  print_endline "  -c, --compile-only: no code generation";
  print_endline "  -Idir, --include=dir : append dir to include path";
  print_endline "  --import=file.flxh : automatically #import <file.flxh>";
  print_endline "  --syntax=file.flxh : use syntax <file.flxh>";
  print_endline "  --automaton=syntax.automaton : parser automaton filename";
  print_endline "  --inline, --noinline, --optimise";
  print_endline "  --cache_dir=<none>: .par and .syncache directory";
  print_endline "  --output_dir=<none>: .cpp, .hpp, .why etc directory";
  print_endline "  --bundle_dir=<none>: output files needed for C++ compilation to this folder";
  print_endline "  --force : force recompilation";
  print_endline "  --optimise: enable some expensive optimisations";
  print_endline "  --with-comments : generate code with comments";
  print_endline "  --mangle-names : generate code with fully mangled names";
  print_endline "  --time : show processing time for each stage";
  print_endline "  --automaton=syntax.automaton: parser automaton"


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
    print_endline ("Felix version " ^ !version_data.version_string);
    exit 0
  end;

  (* Now extract the driver options. *)
  let compiler_options = get_options raw_options in

  (* Error out if we didn't specify any files. *)
  if compiler_options.files = [] then begin
    print_options ();
    exit 1
  end;

  if compiler_options.print_flag then
  print_endline ("// Include directories = " ^
    String.concat " " compiler_options.include_dirs);

  (* Make sure the current directory is in the search path. *)
  let include_dirs =
    Filename.current_dir_name :: compiler_options.include_dirs
  in
  let compiler_options = { compiler_options with
    include_dirs = include_dirs }
  in
  if compiler_options.optimise then print_endline "Hyperlight optimisation on";
  compiler_options


