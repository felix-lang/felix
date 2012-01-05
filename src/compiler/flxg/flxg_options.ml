open Format

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


let get_options options =
  let compiler_phase =
    match get_key_value options "compiler-phase" with
    | None -> Phase_codegen
    | Some phase ->
        match phase with
        | "parse" -> Phase_parse
        | "desugar" -> Phase_desugar
        | "bind" -> Phase_bind
        | "optimize" -> Phase_optimize
        | "lower" -> Phase_lower
        | "codegen" -> Phase_codegen
        | _ -> failwith "Invalid value for --compiler-phase"
  in
  let include_dirs= fixup (get_keys_values options ["I"; "include"]) in
  {
    compiler_phase = compiler_phase;
    optimise    = check_keys options ["opt"; "optimise"];
    debug       = check_key options "debug";
    with_comments = check_key options "with-comments";
    mangle_names = check_key options "mangle-names";
    include_dirs= include_dirs;
    print_flag  = check_keys options ["v"; "verbose"];
    generate_axiom_checks  = not (check_keys options ["no-check-axioms"]);
    trace       = check_keys options ["trace"];
    files       = List.rev (get_key_values options "");
    raw_options = options;
    reverse_return_parity = check_key options "e";
    force_recompile = check_keys options ["force"];
    cache_dir = get_key_value options "cache_dir";
    output_dir = get_key_value options "output_dir";
    max_inline_length =
      begin
        let inline =
          match get_key_value options "inline" with
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
            begin match check_key options "noinline" with
            | true -> 0
            | false ->
              begin match check_keys options ["inline";"opt";"optimise"] with
              | true -> 250
              | false -> 15
              end
            end
        in
          (* we need to at least inline a little for typeclasses *)
          if inline < 10 then 10 else inline
      end
    ;
    auto_imports = get_key_values options "import";
    syntax= get_key_values options "syntax";
    automaton_filename= 
      begin match get_key_value options "automaton" with
      | Some a -> a
      | None ->
        begin 
          try 
            Flx_filesys.find_file ~include_dirs "syntax.automaton" 
          with Flx_filesys.Missing_path _ -> 
          try 
            let d = Flx_filesys.find_dir ~include_dirs "grammar" in
            Flx_filesys.join d "syntax.automaton"
          with Flx_filesys.Missing_path _ -> "syntax.automaton" (* current directory *)
        end
      end
    ;

    compile_only = check_keys options ["c";"compile-only"]
  }


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
  print_endline "  --force : force recompilation";
  print_endline "  --with-comments : generate code with comments";
  print_endline "  --mangle-names : generate code with fully mangled names"


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
  let compiler_options = get_options raw_options in

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
