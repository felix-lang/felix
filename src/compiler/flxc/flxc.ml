(*
 * Main driver code
 *)

type state_t = {
  syms: Flx_mtypes2.sym_state_t;
  macro_state: Flx_macro.macro_state_t;
  desugar_state: Flx_desugar.desugar_state_t;
  symtab: Flx_symtab.t;
  bind_state: Flx_bind.bind_state_t;
  module_index: Flx_types.bid_t;
  init_index: Flx_types.bid_t;
}


(* Create the global state needed for compilation *)
let create_state options =
  (* Construct the state needed for compilation *)
  let syms = Flx_mtypes2.make_syms options in
  let symtab = Flx_symtab.make syms in

  (* Declare the module to work within *)
  let module_index, _ = Flx_symtab.add_dcl symtab (
    Flx_srcref.dummy_sr, "", None, `Public, Flx_ast.dfltvs,
    Flx_types.DCL_module [])
  in
  let module_symbol = Hashtbl.find syms.Flx_mtypes2.sym_table module_index in

  (* Find the module's _init_ function *)
  let init_index =
    match Hashtbl.find module_symbol.Flx_types.pubmap "_init_" with
    | Flx_types.FunctionEntry [ { Flx_types.base_sym=base_sym } ] -> base_sym
    | _ -> assert false
  in

  {
    syms = syms;
    macro_state = Flx_macro.make_macro_state "<input>";
    desugar_state = Flx_desugar.make_desugar_state "<input>" syms;
    symtab = symtab;
    bind_state = Flx_bind.make_bind_state
      ~parent:module_index
      ~env:(Flx_lookup.build_env syms (Some init_index))
      syms;
    module_index = module_index;
    init_index = init_index;
  }


(* Process the stdin input statements *)
let parse_stmt state stmt () =
  print_endline ("... PARSED:    " ^ (Flx_print.string_of_statement 0 stmt))

let expand_macros_in_stmt state stmt () =
  Flx_macro.expand_macros_in_statement state.macro_state begin fun () stmt ->
    print_endline ("... EXPANDED:  " ^ (Flx_print.string_of_statement 0 stmt));
  end () stmt

let desugar_stmt state stmt () =
  Flx_desugar.desugar_statement state.desugar_state begin fun () asm ->
    print_endline ("... DESUGARED: " ^ (Flx_print.string_of_asm 0 asm));
  end () stmt

(* Convenience function to make bsym_table from our state. *)
let make_bind_state state =
  let bsym_table = Hashtbl.create 97 in

  let module_symbol = Hashtbl.find
    state.syms.Flx_mtypes2.sym_table
    state.module_index
  in
  let init_symbol = Hashtbl.find
    state.syms.Flx_mtypes2.sym_table
    state.init_index
  in

  (* Bind the module and init function. *)
  ignore (Flx_bbind.bbind_symbol
    state.syms
    bsym_table
    state.module_index
    module_symbol);
  ignore (Flx_bbind.bbind_symbol
    state.syms
    bsym_table
    state.init_index
    init_symbol);

  (* Add the module and init function to the child map *)
  let child_map = Flx_child.make () in
  begin match module_symbol.Flx_types.parent with
  | Some parent -> Flx_child.add_child child_map parent state.module_index;
  | None -> ()
  end;

  begin match init_symbol.Flx_types.parent with
  | Some parent -> Flx_child.add_child child_map parent state.init_index;
  | None -> ()
  end;

  bsym_table, child_map


(* Helper function to simplify binding a statement. *)
let bind_stmt' state bsym_table stmt =
  (* First bind the statement. *)
  let bids, bexes =
    Flx_desugar.desugar_statement state.desugar_state begin fun bs asm ->
      Flx_bind.bind_asm state.bind_state bsym_table begin fun (bids, bexes) b ->
        match b with
        | Flx_bind.Bound_symbol (bid,_) -> bid :: bids, bexes
        | Flx_bind.Bound_exe bexe -> bids, bexe :: bexes
      end bs asm
    end ([], []) stmt
  in

  (* Reverse the bound symbol lists so we can make tail calls. *)
  List.rev bids, List.rev bexes


(* Helper function to print to bsyms. *)
let print_bids state bsym_table bids =
  List.iter begin fun bid ->
    match Flx_hashtbl.find bsym_table bid with
    | None -> ()
    | Some (_,_,_,bbdcl) ->
        print_endline ("... BOUND SYMBOL:     " ^ Flx_print.string_of_bbdcl
          state.syms.Flx_mtypes2.sym_table
          bsym_table
          bbdcl
          bid)
  end bids


(* Helper function to help print out bexes. *)
let print_bexes state bsym_table bexes =
  List.iter begin fun bexe ->
    print_endline ("... BOUND EXE:     " ^ Flx_print.string_of_bexe
      state.syms.Flx_mtypes2.sym_table
      bsym_table
      0
      bexe)
  end bexes


let bind_stmt state =
  let bsym_table, _ = make_bind_state state in

  fun stmt () ->
    let bids, bexes = bind_stmt' state bsym_table stmt in
    print_bids state bsym_table bids;
    print_bexes state bsym_table bexes


(* Helper function to simplify optimizing values. *)
let optimize_stmt' state bsym_table child_map stmt =
  (* Bind the bsyms and bexes. *)
  let bids, bexes = bind_stmt' state bsym_table stmt in

  (* Optimize the bsyms and bexes. *)
  Flx_opt.optimize state.syms bsym_table child_map state.init_index bids bexes


let optimize_stmt state =
  (* Create a child map of the symbols. *)
  let bsym_table, child_map = make_bind_state state in

  (* Make references of the bsym_table and child map since we'll be replacing these
   * values as we optimize. *)
  let bsym_table = ref bsym_table in
  let child_map = ref child_map in

  (* Return a function that processes a statement at a time. *)
  fun stmt () ->
    let bsym_table', child_map', bids, bexes = optimize_stmt'
      state
      !bsym_table
      !child_map
      stmt
    in
    bsym_table := bsym_table';
    child_map := child_map';

    print_bids state !bsym_table bids;
    print_bexes state !bsym_table bexes


let lower_stmt' state bsym_table child_map lower_state stmt =
  (* Optimize the bsyms and bexes. *)
  let bsym_table, child_map, bids, bexes = optimize_stmt'
    state
    bsym_table
    child_map
    stmt
  in

  (* Then, lower the bsyms and bexes. *)
  Flx_lower.lower lower_state bsym_table child_map state.init_index bids bexes


let lower_stmt state =
  (* Create a child map of the symbols. *)
  let bsym_table, child_map = make_bind_state state in

  (* Make references of the bsym_table and child map since we'll be replacing these
   * values as we optimize. *)
  let bsym_table = ref bsym_table in
  let child_map = ref child_map in

  (* Create the stat needed for lowering symbols. *)
  let lower_state = Flx_lower.make_lower_state state.syms in

  (* Return a function that processes a statement at a time. *)
  fun stmt () ->
    let bsym_table', child_map', bids, bexes = lower_stmt'
      state
      !bsym_table
      !child_map
      lower_state
      stmt
    in
    bsym_table := bsym_table';
    child_map := child_map';

    print_bids state !bsym_table bids;
    print_bexes state !bsym_table bexes


let compile_stmt state =
  (* Create a child map of the symbols. *)
  let bsym_table, child_map = make_bind_state state in

  (* Make references of the bsym_table and child map since we'll be replacing these
   * values as we optimize. *)
  let bsym_table = ref bsym_table in
  let child_map = ref child_map in

  (* Create the stat needed for lowering symbols. *)
  let lower_state = Flx_lower.make_lower_state state.syms in

  (* Initialize the native jit. *)
  ignore (Llvm_executionengine.initialize_native_target ());

  (* Make the state needed for code generation. *)
  let codegen_state = Flx_codegen.make_codegen_state
    state.syms
    !Options.optimize
  in

  (* Return a function that processes a statement at a time. *)
  fun stmt () ->
    let bsym_table', child_map', bids, bexes = lower_stmt'
      state
      !bsym_table
      !child_map
      lower_state
      stmt
    in
    bsym_table := bsym_table';
    child_map := child_map';

    (* Generate code for the exes and bsyms. *)
    let f =
      if !Options.phase = Options.Run
      then Flx_codegen.codegen_and_run
      else Flx_codegen.codegen
    in

    ignore (f codegen_state !bsym_table !child_map bids bexes)


(* Parse all the imports *)
let parse_imports handle_stmt init =
  let parser_state =
    List.fold_left begin fun parser_state name ->
      Flx_parse.parse_file
        ~include_dirs:!Options.include_dirs
        parser_state
        name
    end
    (Flx_parse.make_parser_state handle_stmt init)
    (List.rev !Options.imports)
  in

  (*
  (* The statements are constructed in reverse *)
  let stmts = List.rev stmts in

  (* Desugar all the statements *)
  let asms = Flx_desugar.desugar_compilation_unit state.desugar_state stmts in

  (* Create a symbol table from those desugared statements *)
  let _ = Flx_symtab.add_asms state.symtab asms in

  (*
  (* Now, bind all the symbols *)
  let bsym_table = Flx_bbind.bbind state.bbind_state in
  *)
  *)

  parser_state


(* Parse stdin *)
let rec parse_stdin (handle_stmt, init, local_data) =
  (* Wrap the handle_stmt so that we can print '>>>'. *)
  let handle_stmt stmt init =
    let result = handle_stmt stmt init in
    print_string ">>> "; flush stdout;
    result
  in
  let parser_state = (handle_stmt, init, local_data) in

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
        (* parse_stdin parser_state; *)
        parser_state
    | Flx_exceptions.ClientErrorn (_, e)
    | Flx_exceptions.ClientError2 (_, _, e)
    | Flx_exceptions.ClientError (_, e) ->
        print_endline e;
        (* parse_stdin parser_state; *)
        parser_state
  in
  parser_state


let main () =
  let options = Options.make_felix_compiler_options () in
  let state = create_state options in

  (* Parse all the imported files and get the desugared statements *)
  let _, asms, local_data = parse_imports (bind_stmt state) () in

  (* Parse stdin and get the desugared statements *)
  begin match !Options.phase with
  | Options.Parse ->
      ignore (parse_stdin ((parse_stmt state), asms, local_data))

  | Options.ExpandMacros ->
      ignore (parse_stdin ((expand_macros_in_stmt state), asms, local_data))

  | Options.Desugar ->
      ignore (parse_stdin ((desugar_stmt state), asms, local_data))

  | Options.Bind ->
      ignore (parse_stdin ((bind_stmt state), asms, local_data));

  | Options.Optimize ->
      ignore (parse_stdin ((optimize_stmt state), asms, local_data));

  | Options.Lower ->
      ignore (parse_stdin ((lower_stmt state), asms, local_data));

  | Options.Compile | Options.Run ->
      ignore (parse_stdin ((compile_stmt state), asms, local_data))
  end;

  0
;;


exit (main ())
