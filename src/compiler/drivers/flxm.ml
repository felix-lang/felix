(* macro test harness *)

open Flx_util
open Flx_macro
open Flx_print
open Flx_ast
open Flx_getopt
open Flx_version
open Flx_flxopt
open Flx_types
open Flx_set
open Flx_mtypes2
;;
Flx_version_hook.set_version ();;

let print_help () = print_options(); exit(0)
;;
let reverse_return_parity = ref false
;;
try
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
  let input_file_name = filebase ^ ".flx"
  and iface_file_name = filebase ^ ".fix"
  and module_name =
    let n = String.length filebase in
    let i = ref (n-1) in
    while !i <> -1 && filebase.[!i] <> '/' do decr i done;
    String.sub filebase (!i+1) (n - !i - 1)
  in

  (* PARSE THE IMPLEMENTATION FILE *)
  print_endline ("//Parsing Implementation " ^ input_file_name);
  let parser_state = List.fold_left
    (Flx_parse.parse_file ~include_dirs:compiler_options.include_dirs)
    (Flx_parse.make_parser_state (fun stmt stmts -> stmt :: stmts) [])
    (compiler_options.auto_imports @ [input_file_name])
  in
  let parse_tree = List.rev (Flx_parse.parser_data parser_state) in
  print_endline (Flx_print.string_of_compilation_unit parse_tree);
  print_endline "//PARSE OK";

  print_endline "//----------------------------";
  print_endline "//IMPLEMENTATION EXPANDED:";

  let local_prefix = module_name in
  let expanded = expand_macros local_prefix 5000 parse_tree in
  print_endline (Flx_print.string_of_compilation_unit expanded);
  print_endline "//----------------------------";

with x -> Flx_terminate.terminate !reverse_return_parity x
;;
