open Format

open Flx_options
open Flx_mtypes2
open Flxg_state


(** Parse an implementation file *)
let parse_syntax state =
  let include_dirs = state.syms.compiler_options.include_dirs in
  let synfiles = List.concat
    (List.map
      (Flx_colns.render include_dirs)
      state.syms.compiler_options.syntax)
  in
  (* print_endline ("//Parsing syntax " ^ String.concat ", " synfiles); *)
  let parser_state = List.fold_left
    (fun state file -> Flx_parse.parse_syntax_file ~include_dirs state file)
    (Flx_parse.make_parser_state ())
    (synfiles)
  in

  let auto_imports = List.concat (List.map
    (Flx_colns.render include_dirs)
    state.syms.compiler_options.auto_imports)
  in

  let parser_state = List.fold_left
    (fun state file -> Flx_parse.parse_file ~include_dirs state file)
    parser_state
    auto_imports
  in

  let parsing_device =
    !(Flx_parse_helper.global_data.Flx_token.parsing_device)
  in
  fprintf state.ppf "PARSED SYNTAX/IMPORT FILES@.";

  let oc = open_out_bin state.syms.compiler_options.automaton_filename in
  Marshal.to_channel oc parser_state [];
  Marshal.to_channel oc parsing_device [];
  close_out oc;
  fprintf state.ppf "Saved automaton to disk@.";

  parser_state

let load_syntax state =
  try
     let filename = state.syms.compiler_options.automaton_filename in
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

    Flx_profile.call "Flxg_parse.parse_syntax" parse_syntax state

(** Parse an implementation file *)
let parse_file state parser_state file =
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

  stmts
