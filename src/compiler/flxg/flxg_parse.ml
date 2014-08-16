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
    (fun state file -> 
      let hash_includes, local_data = 
        Flx_parse_driver.parse_file_with_syntax_unit ~include_dirs state file
      in local_data
    )
    (Flx_parse_driver.make_parser_state ())
    (synfiles)
  in

  let auto_imports = List.concat (List.map
    (Flx_colns.render include_dirs)
    state.syms.compiler_options.auto_imports)
  in

  let parser_state = List.fold_left
    (fun state file -> 
      let hash_includes, local_data =
        Flx_parse_driver.parse_file_with_compilation_unit ~include_dirs state file
      in local_data
    )
    parser_state
    auto_imports
  in

  let parsing_device =
    !(Flx_parse_helper.global_data.Flx_token.parsing_device)
  in
  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
  print_endline "PARSED SYNTAX/IMPORT FILES.";

    let filename =state.syms.compiler_options.automaton_filename  in
    Flx_filesys.mkdirs (Filename.dirname filename);
print_endline ("Trying to store automaton " ^ filename);
  let oc = 
    try Some ( open_out_bin filename)
    with _ -> None
  in
  begin match oc with
  | Some oc ->
    Marshal.to_channel oc parser_state [];
    Marshal.to_channel oc parsing_device [];
    close_out oc;
    print_endline "Saved automaton to disk.";
  | None ->
    print_endline ("Can't store automaton to disk file " ^ filename);
  end
  ;
  parser_state

let load_syntax state =
  if state.syms.compiler_options.force_recompile then
    Flx_profile.call "Flxg_parse.parse_syntax" parse_syntax state
  else
  let filename = state.syms.compiler_options.automaton_filename in
  try
(* print_endline ("Trying to load automaton " ^ filename); *)
     let oc = open_in_bin filename in
     let local_data = Marshal.from_channel oc in
     let parsing_device = Marshal.from_channel oc in
     close_in oc;
     (* print_endline "Loaded automaton from disk"; *)
     let env = Flx_parse_helper.global_data.Flx_token.env in
     let scm = local_data.Flx_token.scm in
     Flx_dssl.load_scheme_defs env scm;
     Flx_parse_helper.global_data.Flx_token.parsing_device := parsing_device;
     local_data
  with _ ->
    print_endline ("Can't load automaton '"^filename^"'from disk: building!");

    Flx_profile.call "Flxg_parse.parse_syntax" parse_syntax state

(** Parse an implementation file *)
let parse_file state parser_state file =
(*
print_endline ("DEBUG: flxg_parse.parse_file counter="^string_of_int (!(state.syms.counter))^", filename input=" ^ file);
*)
  let file_name =
    if Filename.check_suffix file ".flx" then file else 
    if Filename.check_suffix file ".fdoc" then file else 
    let flxt = Flx_filesys.virtual_filetime 0.0 (file ^ ".flx") in
    let fdoct = Flx_filesys.virtual_filetime 0.0 (file ^ ".fdoc") in
    if fdoct > flxt then file ^ ".fdoc" else file ^ ".flx"
  in
  let local_prefix = Filename.chop_suffix (Filename.basename file_name) ".flx" in
  let include_dirs = state.syms.compiler_options.include_dirs in
  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
  print_endline  ("//Parsing Implementation " ^ file_name);
  if state.syms.compiler_options.print_flag then print_endline ("Parsing " ^ file_name);
  let parser_state = 
    let hash_includes, local_data = 
      Flx_parse_driver.parse_file_with_compilation_unit ~include_dirs parser_state file_name 
    in local_data
  in

(*
  let stmts = List.rev (Flx_parse.parser_data parser_state) in
*)
  let stmts = List.rev_map (fun scm -> Flx_colns.ocs2flx scm) (Flx_parse_driver.parser_data parser_state) in
(*
print_endline (    "DEBUG: AFTER PARSE: flxg_parse.parse_file counter="^string_of_int (!(state.syms.counter))^", filename input=" ^ file);
*)
(*
  let macro_state = Flx_macro.make_macro_state local_prefix state.syms.counter in
  let stmts = Flx_macro.expand_macros macro_state stmts in
print_endline (    "DEBUG: AFTER MACRO: flxg_parse.parse_file counter="^string_of_int (!(state.syms.counter))^", filename input=" ^ file);
*)
  stmts
