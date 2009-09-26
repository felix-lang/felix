(*
 * Main driver code
 *)

type state_t = {
  syms: Flx_mtypes2.sym_state_t;
  macro_state: Flx_macro.macro_state_t;
  desugar_state: Flx_desugar.desugar_state_t;
  symtab: Flx_symtab.t;
  bind_state: Flx_bind.bind_state_t;
  frontend_state: Flx_frontend.frontend_state_t;
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
    frontend_state = Flx_frontend.make_frontend_state syms;
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

let bind_stmt state =
  let child_map = Flx_child.make () in
  let bsym_table = ref (Hashtbl.create 97) in

  fun stmt () ->
  Flx_desugar.desugar_statement state.desugar_state begin fun () asm ->
    Flx_bind.bind_asm state.bind_state !bsym_table begin fun () bound_value ->
      match bound_value with
      | Flx_bind.Bound_exe bexe ->
          print_endline ("... BOUND EXE:     " ^ Flx_print.string_of_bexe
            state.syms.Flx_mtypes2.sym_table
            !bsym_table
            0
            bexe)

      | Flx_bind.Bound_symbol (index, ((_,parent,_,e) as symbol)) ->
          print_endline ("... BOUND SYMBOL:     " ^ Flx_print.string_of_bbdcl
            state.syms.Flx_mtypes2.sym_table
            !bsym_table
            e
            index);

    end () asm
  end () stmt


let compile_bexe
  state
  codegen_state
  bsym_table
  the_module
  context
  the_fpm
  the_ee
  bexe
=
  (* Make a new function to execute the statement in. We use an opaque
   * type so that we can later refine it to the actual value of the
   * returned expression. *)
  let the_function = Llvm.declare_function
    ""
    (Llvm.function_type (Llvm.void_type context) [||])
    the_module
  in

  (* Create the initial basic block *)
  let bb = Llvm.append_block context "entry" the_function in
  let builder = Llvm.builder_at_end context bb in

  let e = Flx_codegen.codegen_bexe codegen_state bsym_table builder bexe in

  (* Make sure we have a return at the end of the function. *)
  ignore (Llvm.build_ret_void builder);

  (* Make sure the function is valid. *)
  Llvm_analysis.assert_valid_function the_function;

  (* Optimize the function. *)
  ignore (Llvm.PassManager.run_function the_function the_fpm);

  Llvm.dump_module the_module;

  if !Options.phase = Options.Run then begin
    (* Execute the statement. *)
    ignore (Llvm_executionengine.ExecutionEngine.run_function
      the_function
      [||]
      the_ee)
  end


let compile_stmt state =
  (* Create the llvm state *)

  (* Initialize the native jit. *)
  ignore (Llvm_executionengine.initialize_native_target ());

  let context = Llvm.create_context () in
  let the_module = Llvm.create_module context "__root__" in

  (* Set up the llvm optimizer and execution engine *)
  let the_module_provider = Llvm.ModuleProvider.create the_module in
  let the_ee =
    Llvm_executionengine.ExecutionEngine.create the_module_provider in
  let the_fpm = Llvm.PassManager.create_function the_module_provider in

  (* Set up the optimizer pipeline.  Start with registering info about how the
   * target lays out data structures. *)
  Llvm_target.TargetData.add
  (Llvm_executionengine.ExecutionEngine.target_data the_ee)
  the_fpm;

  if !Options.optimize >= 1 then begin
    (* Promote allocas to registers. *)
    Llvm_scalar_opts.add_memory_to_register_promotion the_fpm;

    (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
    Llvm_scalar_opts.add_instruction_combining the_fpm;

    (* reassociate expressions. *)
    Llvm_scalar_opts.add_reassociation the_fpm;

    (* Eliminate Common SubExpressions. *)
    Llvm_scalar_opts.add_gvn the_fpm;

    (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
    Llvm_scalar_opts.add_cfg_simplification the_fpm;
  end;

  ignore (Llvm.PassManager.initialize the_fpm);

  let codegen_state = Flx_codegen.make_codegen_state
    state.syms
    context
    the_module
    the_fpm
    the_ee
  in

  (* Create a child map of the symbols. *)
  let bsym_table = ref (Hashtbl.create 97) in
  let child_map = ref (Flx_child.make ()) in

  (* Return a function that processes a statement at a time. *)
  fun stmt () ->

  (* First bind the statement. *)
  let bexes, bids =
    Flx_desugar.desugar_statement state.desugar_state begin fun bs asm ->
      Flx_bind.bind_asm state.bind_state !bsym_table begin fun (bexes, bids) b ->
        match b with
        | Flx_bind.Bound_exe bexe -> bexe :: bexes, bids
        | Flx_bind.Bound_symbol (bid,_) -> bexes, bid :: bids
      end bs asm
    end ([], []) stmt
  in

  (* Reverse the bound symbol lists so we can make tail calls. *)
  let bexes = List.rev bexes in
  let bids = List.rev bids in

  (* Lower and optimize the symbols. *)
  let bsym_table', child_map', bexes, bids = Flx_frontend.lower_symbols
    state.frontend_state
    !bsym_table
    !child_map
    state.module_index
    bexes
    bids
  in
  bsym_table := bsym_table';
  child_map := child_map';

  (* Next, do the code generation. First we'll generate the symbols. *)
  List.iter begin fun bid ->
    let (_,parent,_,bbdcl) as bsym = Hashtbl.find !bsym_table bid in

    print_endline ("... BOUND SYM:     " ^ Flx_print.string_of_bbdcl
      state.syms.Flx_mtypes2.sym_table
      !bsym_table
      bbdcl
      bid);
    print_newline ();

    (* Only codegen top-level symbols, since that'll be handled by the code
     * generator. *)
    match parent with
    | Some parent -> ()
    | None -> Flx_codegen.codegen_symbol codegen_state !bsym_table bid bsym
  end bids;

  (* Finally, generate code for the executions. *)
  List.iter begin fun bexe ->
    print_endline ("... BOUND EXE:     " ^ Flx_print.string_of_bexe
      state.syms.Flx_mtypes2.sym_table
      !bsym_table
      0
      bexe);
    print_newline ();

    compile_bexe
      state
      codegen_state
      !bsym_table
      the_module
      context
      the_fpm
      the_ee
      bexe
  end bexes


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
        parse_stdin parser_state;
    | Flx_exceptions.ClientErrorn (_, e)
    | Flx_exceptions.ClientError2 (_, _, e)
    | Flx_exceptions.ClientError (_, e) ->
        print_endline e;
        parse_stdin parser_state;
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

  | Options.Compile | Options.Run ->
      ignore (parse_stdin ((compile_stmt state), asms, local_data))
  end;

  0
;;


exit (main ())
