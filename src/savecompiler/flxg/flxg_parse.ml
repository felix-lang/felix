open Flx_options
open Flx_mtypes2
open Flxg_state


(* -------------------------------------------------------------------------- *)
(** Parse an implementation file. *)
let parse_syntax state =
  let include_dirs = state.syms.compiler_options.include_dirs in

  (* Get native file paths for each path in include directory. *)
  let synfiles = 
    List.concat
      (List.map
        (Flx_colns.render include_dirs)
        state.syms.compiler_options.syntax)
  in

  (* Do the parsing. *)
(* print_endline ("//Parsing syntax " ^ String.concat ", " synfiles); *)
  let parser_state = 
    List.fold_left
      (fun state file -> 
        let hash_includes, local_data = 
          Flx_parse_driver.parse_file_with_syntax_unit ~include_dirs state file
        in local_data)
      (Flx_parse_driver.make_parser_state ())
      synfiles
  in

  (* From the parsing state, take the resultant includes (auto imports) and get native paths of them. *)
  let auto_imports = 
    List.concat (
      List.map
        (Flx_colns.render include_dirs)
        state.syms.compiler_options.auto_imports)
  in

  (* Continue compiling the includes specified in the auto imports. *)
  let parser_state = 
    List.fold_left
      (fun state file -> 
        let hash_includes, local_data =
          Flx_parse_driver.parse_file_with_compilation_unit ~include_dirs state file
        in local_data)
      parser_state
      auto_imports
  in

  (* Dereference the parsing device for storage on disk. *)
  let parsing_device =
    !(Flx_parse_data.global_data.Flx_token.parsing_device)
  in

  (* Debugging. *)
  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
    print_endline "PARSED SYNTAX/IMPORT FILES.";

  (* Create directory for housing the automotons. *)
  let filename = state.syms.compiler_options.automaton_filename  
  in Flx_filesys.mkdirs (Filename.dirname filename);

  (* Debugging *)
  print_endline ("Trying to store automaton " ^ filename);

  (* Open file for binary output of automoton. Saving parsed file objects for later. *)
  let oc = 
    try Some ( open_out_bin filename)
    with _ -> None
  in
  begin match oc with
  | Some oc ->
      (* If we have a file, output parser state to disk. *)
      Marshal.to_channel oc parser_state [];
      Marshal.to_channel oc parsing_device [];
      close_out oc;
      print_endline "Saved automaton to disk.";
  | None ->
      print_endline ("Can't store automaton to disk file " ^ filename);
  end;

  (* end function, return parser state *)
  parser_state



(* -------------------------------------------------------------------------- *)
(** Load the syntax from file or rebuild it. Either way, it goes into memory. *)
let load_syntax state =

  (* Did the programmar enter 'flx --force-compiler'? *)
  if state.syms.compiler_options.force_recompile then
    Flx_profile.call "Flxg_parse.parse_syntax" parse_syntax state

  else
    (* Compile normally. *)
    let filename = state.syms.compiler_options.automaton_filename in

    try
      (* print_endline ("Trying to load automaton " ^ filename); *)

      (* Load atomaton from binary file. *)
      let oc = open_in_bin filename in
      let local_data = Marshal.from_channel oc in
      let parsing_device = Marshal.from_channel oc in
      close_in oc;
      (* print_endline "Loaded automaton from disk"; *)

      (* Create environment for parsing system. *)
      let env = Flx_parse_data.global_data.Flx_token.env in
      let scm = local_data.Flx_token.scm in
      
      (* Load data from disk into environment. *)
      Flx_dssl.load_scheme_defs env scm;
      Flx_parse_data.global_data.Flx_token.parsing_device := parsing_device;
       
      (* Return data. *)
      local_data

    with _ ->
      (* An error occurred loading the syntax automata. *)
      print_endline ("Can't load automaton '"^filename^"'from disk: building!");

      (* Recover by parsing everything instead of relying on the disk image. *)
      Flx_profile.call "Flxg_parse.parse_syntax" parse_syntax state

(* -------------------------------------------------------------------------- *)
(** Parse an implementation file *)
let parse_file state parser_state file =

  (* print_endline 
      ("DEBUG: flxg_parse.parse_file counter="
        ^ string_of_int (!(state.syms.counter))
        ^ ", filename input=" 
        ^ file); 
  *)

  (* Figure out the file to compile. *)
  let file_name =
    if Filename.check_suffix file ".flx" then file else 
    if Filename.check_suffix file ".fdoc" then file else 
    let flxt = Flx_filesys.virtual_filetime 0.0 (file ^ ".flx") in
    let fdoct = Flx_filesys.virtual_filetime 0.0 (file ^ ".fdoc") in
    if fdoct > flxt then file ^ ".fdoc" else file ^ ".flx"
  in
  
  (* Shortcut variables. *)
  let local_prefix = Filename.chop_suffix (Filename.basename file_name) ".flx" in
  let include_dirs = state.syms.compiler_options.include_dirs in

  (* Debugging. *)
  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
    print_endline  ("//Parsing Implementation " ^ file_name);
  if state.syms.compiler_options.print_flag then 
    print_endline ("Parsing " ^ file_name);

  let parser_state = 
    let hash_includes, local_data = 
      Flx_parse_driver.parse_file_with_compilation_unit ~include_dirs parser_state file_name 
    in local_data
  in

  (* let stmts = List.rev (Flx_parse.parser_data parser_state) in *)

  let stmts = 
    List.rev_map 
      (fun scm -> Flx_colns.ocs2flx scm) 
      (Flx_parse_driver.parser_data parser_state) 
  in

(* 
  print_endline ( "DEBUG: AFTER PARSE: flxg_parse.parse_file counter="
      ^ string_of_int (!(state.syms.counter))
      ^ ", filename input=" 
      ^ file);
*)

(* 
  (* The following is deprecated due to removal of generalized macros. *)

  let macro_state = Flx_macro.make_macro_state local_prefix state.syms.counter in
  let stmts = Flx_macro.expand_macros macro_state stmts in

  print_endline ( "DEBUG: AFTER MACRO: flxg_parse.parse_file counter="
    ^ string_of_int (!(state.syms.counter))
    ^ ", filename input=" 
    ^ file);

*)

  (* Return the parsed statements. *)
  stmts



