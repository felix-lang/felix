(* lexer test harness *)

open Flx_mtypes2
open Flx_flxopt
open Flx_getopt
open Flx_types
open Flx_version
open Flx_prelex
open Flx_token
;;
Flx_version_hook.set_version ();;

let print_help () = print_options(); exit(0)
;;

let run() =
  let raw_options = parse_options Sys.argv in
  let compiler_options = get_felix_options raw_options in

  if check_keys raw_options ["h"; "help"]
  then print_help ()
  ;
  if check_key raw_options "version"
  then (print_endline ("Felix Version " ^ !version_data.version_string))
  ;
  if compiler_options.print_flag then begin
    print_string "//Include directories = ";
    List.iter (fun d -> print_string (d ^ " "))
    compiler_options.include_dirs;
    print_endline ""
  end
  ;

  let filename =
    match get_key_value raw_options "" with
    | Some s -> s
    | None -> exit 0
  in
  let filebase = filename in
  let input_file_name = filebase ^ ".flx" in

  if compiler_options.print_flag then begin
    print_endline "---------------------------------------";
    print_endline ("Lexing " ^ input_file_name);
    print_endline "---------------------------------------";
    print_endline "Pre tokens"
  end;

  let pretokens =
    Flx_pretok.pre_tokens_of_filename
    input_file_name
    (Filename.dirname input_file_name)
    compiler_options.include_dirs
    compiler_options.cache_dir
    Flx_macro.expand_expression
    compiler_options.auto_imports
  in
  if compiler_options.print_flag then begin
    Flx_tok.print_pre_tokens  pretokens;
    print_endline "---------------------------------------";
    print_endline "Tokens"
  end
  ;
  let tokens = Flx_lex1.translate pretokens in
  if compiler_options.print_flag then
    Flx_tok.print_tokens tokens;
  if compiler_options.print_flag then begin
    print_endline "---------------------------------------"
  end
  ;
in
  run()
;;
