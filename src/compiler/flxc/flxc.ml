(*
 * Main driver code
 *)

type state_t = {
  syms: Flx_mtypes2.sym_state_t;
  macro_state: Flx_macro.macro_state_t;
  desugar_state: Flx_desugar.desugar_state_t;
}

let print_stmt state stmt () =
  print_endline ("... PARSED:    " ^ (Flx_print.string_of_statement 0 stmt));

  let expanded = Flx_macro.expand_macros_in_statement state.macro_state stmt in
  print_endline ("... EXPANDED:  " ^ (Flx_print.string_of_compilation_unit expanded));

  let desugared = Flx_desugar.desugar_statement state.desugar_state stmt in
  print_endline ("... DESUGARED: " ^ (Flx_print.string_of_desugared desugared));

  print_string ">>> "; flush stdout;
  ()

let evaluate_stmts state name tree =
  Flx_desugar.desugar_compilation_unit state.desugar_state tree

let parse_file state parser_state name =
  let parser_state = Flx_parse.parse_file
    ~include_dirs:!Options.include_dirs
    parser_state
    name
  in
  let tree = evaluate_stmts state name (Flx_parse.parser_data parser_state) in
  parser_state

let rec parse_stdin parser_state =
  print_string ">>> "; flush stdout;
  let _ =
    try
      Flx_parse.parse_channel
        ~name:"<input>"
        parser_state
        stdin;
    with
    | Dyp.Syntax_error ->
        print_endline "Syntax error";
        parse_stdin parser_state;
  in
  parser_state

let main () =
  Options.parse_args ();
  let options = Options.make_felix_compiler_options () in
  let syms = Flx_mtypes2.make_syms options in
  let state = {
    syms = syms;
    macro_state = Flx_macro.make_macro_state "<input>";
    desugar_state = Flx_desugar.make_desugar_state "<input>" syms; }
  in
  (* Parse all the imports *)
  let parser_state = List.fold_left
    (parse_file state)
    (Flx_parse.make_parser_state (fun stmt stmts -> stmt :: stmts) [])
    (List.rev !Options.imports)
  in
  let parser_state =
    let f, init, local_data = parser_state in
    (* Evaluate all the imported statements *)

    (print_stmt state), (), local_data
  in
  let parser_state = parse_stdin parser_state in
  0
;;

exit (main ())
