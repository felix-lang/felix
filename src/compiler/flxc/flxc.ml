(*
 * Main driver code
 *)

type state_t = {
  syms: Flx_mtypes2.sym_state_t;
  bbdfns: Flx_types.fully_bound_symbol_table_t;
  macro_state: Flx_macro.macro_state_t;
  desugar_state: Flx_desugar.desugar_state_t;
  symtab: Flx_symtab.t;
  bind_state: Flx_bind.bind_state_t;
  child_map: Flx_child.t;
  module_index: int;
  init_index: int;
  context: Llvm.llcontext;
  the_module: Llvm.llmodule;
  the_fpm: [`Function] Llvm.PassManager.t;
  the_execution_engine: Llvm_executionengine.ExecutionEngine.t;
  codegen_state: Flx_codegen.codegen_state_t;
}


(* Create the global state needed for compilation *)
let create_state options =
  (* Construct the state needed for compilation *)
  let syms = Flx_mtypes2.make_syms options in
  let symtab = Flx_symtab.make syms in
  let bbdfns = Hashtbl.create 97 in

  (* Declare the module to work within *)
  let module_index, _ = Flx_symtab.add_dcl symtab (
    Flx_srcref.dummy_sr, "__init_module__", None, `Public, Flx_ast.dfltvs,
    Flx_types.DCL_module [])
  in
  let module_symbol = Hashtbl.find syms.Flx_mtypes2.dfns module_index in

  (* Find the module's _init_ function *)
  let init_index =
    match Hashtbl.find module_symbol.Flx_types.pubmap "_init_" with
    | Flx_types.FunctionEntry [ { Flx_types.base_sym=base_sym } ] -> base_sym
    | _ -> assert false
  in

  (* Create the llvm state *)

  (* Initialize the native jit. *)
  ignore (Llvm_executionengine.initialize_native_target ());

  let context = Llvm.create_context () in
  let the_module = Llvm.create_module context "__root__" in

  (* Set up the llvm optimizer and execution engine *)
  let the_module_provider = Llvm.ModuleProvider.create the_module in
  let the_execution_engine =
    Llvm_executionengine.ExecutionEngine.create the_module_provider in
  let the_fpm = Llvm.PassManager.create_function the_module_provider in

  (* Set up the optimizer pipeline.  Start with registering info about how the
   * target lays out data structures. *)
  Llvm_target.TargetData.add
  (Llvm_executionengine.ExecutionEngine.target_data the_execution_engine)
  the_fpm;

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

  ignore (Llvm.PassManager.initialize the_fpm);

  {
    syms = syms;
    bbdfns = bbdfns;
    macro_state = Flx_macro.make_macro_state "<input>";
    desugar_state = Flx_desugar.make_desugar_state "<input>" syms;
    symtab = symtab;
    bind_state = Flx_bind.make_bind_state
      ~parent:module_index
      ~env:(Flx_lookup.build_env syms (Some init_index))
      syms
      bbdfns;
    child_map = Flx_child.make ();
    module_index = module_index;
    init_index = init_index;
    context = context;
    the_module = the_module;
    the_fpm = the_fpm;
    the_execution_engine = the_execution_engine;
    codegen_state = Flx_codegen.make_codegen_state
      syms
      bbdfns
      context
      the_module
      the_fpm
      the_execution_engine;
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

let bind_stmt state stmt () =
  Flx_desugar.desugar_statement state.desugar_state begin fun () asm ->
    Flx_bind.bind_asm state.bind_state begin fun () bound_value ->
      match bound_value with
      | Flx_bind.Bound_exe bexe ->
          print_endline ("... BOUND EXE:     " ^ Flx_print.string_of_bexe
            state.syms.Flx_mtypes2.dfns
            state.bbdfns
            0
            bexe)

      | Flx_bind.Bound_symbol (index, ((_,parent,_,e) as symbol)) ->
          print_endline ("... BOUND SYMBOL:     " ^ Flx_print.string_of_bbdcl
            state.syms.Flx_mtypes2.dfns
            state.bbdfns
            e
            index);

          (* Add the symbol to the child map. *)
          begin
            match parent with
            | Some parent -> Flx_child.add_child state.child_map parent index
            | None -> ()
          end;

          Flx_typeclass.typeclass_instance_check_symbol
            state.syms
            state.bbdfns
            state.child_map
            index
            symbol;

    end () asm
  end () stmt


let compile_bexe state bexe =
  (* Make a new function to execute the statement in. We use an opaque
   * type so that we can later refine it to the actual value of the
   * returned expression. *)
  let the_function = Llvm.declare_function
    ""
    (Llvm.function_type (Llvm.void_type state.context) [||])
    state.the_module
  in

  (* Create the initial basic block *)
  let bb = Llvm.append_block state.context "entry" the_function in
  let builder = Llvm.builder_at_end state.context bb in

  let e = Flx_codegen.codegen_bexe
    state.codegen_state
    builder
    bexe
  in

  (* Make sure we have a return at the end of the function. *)
  ignore (Llvm.build_ret_void builder);

  (* Make sure the function is valid. *)
  Llvm_analysis.assert_valid_function the_function;

  (* Optimize the function. *)
  ignore (Llvm.PassManager.run_function the_function state.the_fpm);

  Llvm.dump_module state.the_module;

  (* Execute the statement. *)
  ignore (Llvm_executionengine.ExecutionEngine.run_function
    the_function
    [||]
    state.the_execution_engine)


let compile_stmt state stmt () =
  Flx_desugar.desugar_statement state.desugar_state begin fun () asm ->
    Flx_bind.bind_asm state.bind_state begin fun () bound_value ->
      match bound_value with
      | Flx_bind.Bound_exe bexe ->
          print_endline ("... BOUND EXE:     " ^ Flx_print.string_of_bexe
            state.syms.Flx_mtypes2.dfns
            state.bbdfns
            0
            bexe);
          print_newline ();

          compile_bexe state bexe

      | Flx_bind.Bound_symbol (index, ((_,parent,_,e) as symbol)) ->
          print_endline ("... BOUND SYM:     " ^ Flx_print.string_of_bbdcl
            state.syms.Flx_mtypes2.dfns
            state.bbdfns
            e
            index);
          print_newline ();

          (* Add the symbol to the child map. *)
          begin
            match parent with
            | Some parent -> Flx_child.add_child state.child_map parent index
            | None -> ()
          end;

          Flx_typeclass.typeclass_instance_check_symbol
            state.syms
            state.bbdfns
            state.child_map
            index
            symbol;

          (* Only codegen top-level symbols. *)
          match parent with
          | Some parent -> ()
          | None -> Flx_codegen.codegen_symbol
            state.codegen_state
            index
            symbol
    end () asm
  end () stmt


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
  let bbdfns = Flx_bbind.bbind state.bbind_state in
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

      Printf.printf "\n\nbound symbols: %d\n" (Hashtbl.length state.bbdfns);

      (* And print out the bound values *)
      Hashtbl.iter begin fun index (name,parent,sr,entry) ->
        print_endline (
          string_of_int index ^ " --> " ^
          Flx_print.string_of_bbdcl
            state.syms.Flx_mtypes2.dfns
            state.bbdfns
            entry
            index
        )
      end state.bbdfns;

  | Options.Compile ->
      ignore (parse_stdin ((compile_stmt state), asms, local_data));

      Printf.printf "\n\nllvm module:";
      flush stdout;

      (* Print out the generated code. *)
      Llvm.dump_module state.the_module;
  end;

  0
;;


exit (main ())
