(*
 * Main driver code
 *)

type state_t = {
  syms: Flx_mtypes2.sym_state_t;
  macro_state: Flx_macro.macro_state_t;
  desugar_state: Flx_desugar.desugar_state_t;
  bbind_state: Flx_bbind.bbind_state_t;
}


let print_stmt state stmt old_asms =
  print_endline ("... PARSED:    " ^ (Flx_print.string_of_statement 0 stmt));

  let expanded = Flx_macro.expand_macros_in_statement state.macro_state stmt in
  print_endline ("... EXPANDED:  " ^ (Flx_print.string_of_compilation_unit expanded));

  let asms = Flx_desugar.desugar_statement state.desugar_state stmt in
  print_endline ("... DESUGARED: " ^ (Flx_print.string_of_desugared asms));

  (* Bind the variables *)
  let _ = Flx_symtab.build_tables state.syms
    "root"
    Flx_ast.dfltvs
    0
    None
    None
    !(state.syms.Flx_mtypes2.counter)
    asms
  in

  (* Now, bind all the symbols *)
  let bbdfns = Flx_bbind.bbind state.bbind_state in

  print_string " >>> "; flush stdout;
  old_asms @ asms


(* Parse all the imports *)
let parse_imports state =
  let _, stmts, local_data =
    List.fold_left begin fun parser_state name ->
      Flx_parse.parse_file
        ~include_dirs:!Options.include_dirs
        parser_state
        name
    end
    (Flx_parse.make_parser_state (fun stmt stmts -> stmt :: stmts) [])
    (List.rev !Options.imports)
  in

  (* The statements are constructed in reverse *)
  let stmts = List.rev stmts in

  (* Desugar all the statements *)
  let asms = Flx_desugar.desugar_compilation_unit state.desugar_state stmts in

  (* Create a symbol table from those desugared statements *)
  let _ = Flx_symtab.build_tables
    state.syms
    "root"
    Flx_ast.dfltvs
    0
    None
    None
    !(state.syms.Flx_mtypes2.counter)
    asms
  in

  (* Now, bind all the symbols *)
  let bbdfns = Flx_bbind.bbind state.bbind_state in

  asms, local_data


(* Parse stdin *)
let rec parse_stdin parser_state =
  print_string ">>> "; flush stdout;
  let parser_state =
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
  let options = Options.make_felix_compiler_options () in

  (* Construct the state needed for compilation *)
  let state =
    let syms = Flx_mtypes2.make_syms options in
    {
      syms = syms;
      macro_state = Flx_macro.make_macro_state "<input>";
      desugar_state = Flx_desugar.make_desugar_state "<input>" syms;
      bbind_state = Flx_bbind.make_bbind_state syms;
    }
  in

  (* Parse all the imported files and get the desugared statements *)
  let import_asms, local_data = parse_imports state in

  (* Parse stdin and get the desugared statements *)
  let _, stdin_asms, _ = parse_stdin ((print_stmt state), [], local_data) in

  (* Now, bind all the symbols *)
  let bbdfns = Flx_bbind.bbind state.bbind_state in

  print_newline ();
  print_newline ();
  print_endline "before!";

  (* And print out the bound values *)
  Hashtbl.iter begin fun index (name,parent,sr,entry) ->
    print_endline (
      string_of_int index ^ " --> " ^
      Flx_print.string_of_bbdcl state.syms.Flx_mtypes2.dfns bbdfns entry index
    )
  end bbdfns;

  print_endline "after!";

  0
;;

exit (main ())
